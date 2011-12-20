require('rjson'); # JSON parsing
require('bit');   # bit-set 
require('hash');  # hash-table

## transformations from JSON include:
##   naming is more idomatically R (camel-case, no underscores)
##   Run-length-encoded vectors to bitsets
##   feature id's zero-indexed to one-indexed

bitSetOfRLE <- function( RLE ) {
  if ( RLE[[1]] == "RLE" ) {
    rle <- RLE[[2]];
    firstValue <- rle[["first_value"]];
    runLengths <- rle[["run_lengths"]];

    n <- sum( runLengths );
    bitSet <- bit( n );

    v <- firstValue;
    k <- 1;
    j <- 1;
    for( runLength in runLengths ) {
      while ( j < k + runLength ) {
        bitSet[[j]] <- v;
        j <- j + 1;
      };
      k <- j;
      v <- !v; # invert
    };
    bitSet
  }
  else {
    stop( paste("RLE: parser exception with unknown tag", RLE[[1]] ) );
  }
}

tree <- function( v ) {
  tag <- v[[1]]
  if ( tag == "LeafNode" ) {
    list( type=tag, value=v[[2]] )
  }
  else if ( tag == "OrdinalNode" ) {
    cn <- v[[2]];
    list(
         type      = tag,
         featureId = cn[["feature_id"]] + 1,
         split     = cn[["split"]],
         leftTree  = tree( cn[["left_tree"]] ),
         rightTree = tree( cn[["right_tree"]] )
         );
  }
  else if ( tag == "CategoricalNode" ) {
    on <- v[[2]];
    list(
         type      = tag,
         featureId = on[["feature_id"]] + 1,
         goLeft    = bitSetOfRLE( on[["go_left"]] ),
         leftTree  = tree( on[["left_tree"]] ),
         rightTree = tree( on[["right_tree"]] )
         );
  }
  else {
    stop( paste("tree: parser exception with unknown tag", tag));
  }
}

indexByElement <- function( vec ) {
  index <- 1;
  h <- hash();
  for ( element in vec ) {
    h[ element ] <- index;
    index <- index + 1;
  }
  h
}

feature <- function( v ) {
  tag <- v[[1]];
  if ( tag == "CategoricalFeature" ) {
    cf <- v[[2]];
    list(
         type = tag,
         name = cf$name,
         categoryIdByName = indexByElement( cf$categories )
         );
  }
  else if ( tag == "OrdinalFeature" ) {
    of <- v[[2]];
    list(
         type = tag,
         name = of$name,
         domainMin = of[["domain_min"]],
         domainMax = of[["domain_max"]]
         );
  }
  else {
    stop( paste("feature: parser exception with unknown tag", tag) );
  }
}

model <- function( o ) {
  list(
       responseName     = o[["response_name"]],
       positiveCategory = o[["positive_category"]],
       negativeCategory = o[["negative_category"]],
       trees            = lapply( o$trees, tree ),
       features         = lapply( o$features, feature )
       );
}

confusion <- function( o ) {
  list(
       labelTpredT = o[["d_label_T_pred_T"]],
       labelTpredF = o[["d_label_T_pred_F"]],
       labelFpredT = o[["d_label_F_pred_T"]],
       labelFpredT = o[["d_label_F_pred_F"]] );
}
       

checkConsistencyTree <- function( features, tree ) {
  if ( tree$type == "LeafNode" ) {
    TRUE;
  }
  else if ( tree$type == "OrdinalNode" ) {
    if ( tree$featureId < 1 || length(features) < tree$featureId ) {
      stop( paste("feature id not in [ 1 ,", length(features), " ]") );
    }
    else {
      feature <- features[[ tree$featureId ]];
      if ( feature$type == "CategoricalFeature" ) {
        stop( paste("feature ", feature$name, "is categorical, but tree is ordinal"));
      }
      else {
        stopifnot( feature$type == "OrdinalFeature" );
        if ( tree$split < feature$domainMin || tree$split > feature$domainMax ) {
          stop( paste("tree split (", tree$split, ") by feeature", feature$name,
                      "is outside feature domain (", feature$domainMin, ",",
                      feature$domainMax ) );
        }
        else {
          checkConsistencyTree( features, tree$leftTree ) &&
          checkConsistencyTree( features, tree$rightTree );
        }
      }
    }
  }
  else {
    stopifnot( tree$type == "CategoricalNode" );
    if ( tree$featureId < 1 || length(features) < tree$featureId ) {
      stop( paste("feature id not in [ 1 ,", length(features), " ]") );
    }
    else {
      feature <- features[[ tree$featureId ]];
      if ( feature$type == "OrdinalFeature" ) {
        stop( paste("feature", feature$name, "is ordinal, but tree is categorical"));
      }
      else {
        stopifnot( feature$type == "CategoricalFeature" );
        if ( length( feature$categoryIdByName ) != length( tree$goLeft ) ) {
          stop("feature", feature$name, ": invalid categorical partition");
        }
        else {
          checkConsistencyTree( features, tree$leftTree ) &&
          checkConsistencyTree( features, tree$rightTree );
        }
      }
    }
  }
}

checkConsistencyTrees <- function( features, trees ) {
  v <- TRUE;
  for ( tree in trees ) {
    v <- v && checkConsistencyTree( features, tree )
  }
  stopifnot( v );
  TRUE;
}

trainResultV1 <- function( o ) {
  list(
       model=model( o$model ),
       confusion=confusion( o$confusion ),
       auc=o$auc );

}

trainResultOfJson <- function( a ) {
  if ( a[[1]] == "V1" ) {
    trainResultV1( a[[2]] )
  }
  else
    stop( paste("train_result: parser exception with unknown tag", a[[1]] ) );
}


eigendog.trainResult <- function( fileName ) {
  # get the file's size, so we can readChar in one gulp
  fileSize <- file.info( fileName )$size;
  # load contents of file
  fileContents <- readChar( fileName, fileSize );
  # interpret file contents as JSON
  json <- rjson::fromJSON( fileContents, method="C" );
  # convert to train result
  tr <- trainResultOfJson( json );

  # check whether the model is consistent
  checkConsistencyTrees( tr$model$features, tr$model$trees );

  tr
}

predictTree <- function( tree, values ) {
  if ( tree$type == "LeafNode" ) {
    tree$value
  }
  else if ( tree$type == "CategoricalNode" ) {
    value <- values[[ tree$featureId ]];

    # check bit vector whether to go left or right
    if ( tree$goLeft[ value ] ) {
      predictTree( tree$leftTree, values );
    }
    else {
      predictTree( tree$rightTree, values );
    }
  }
  else {
    stopifnot( tree$type == "OrdinalNode" );
    value <- values[[ tree$featureId ]];
    if ( value <= tree$split ) {
      predictTree( tree$leftTree, values );
    }
    else {
      predictTree( tree$rightTree, values );
    }
  }
}

predictTrees <- function( trees, values ) {
  s <- 0.0;
  for( tree in trees ) {
    s <- s + predictTree( tree, values );
  }
  s
}

probability <- function( f ) {
  1. / ( 1. + exp( -2 * f ) )
}

eigendog.predict <- function( model, X, checkOrdinalDomain ) {
  features <- model$features;
  if ( is.null( features ) ) {
    stop( "model has no features" );
  }
  else {
    values <- c();
    for ( feature in features ) {
      # inputs are passed by name; here we order them by feature
      v <- X[[ feature$name ]];
      if ( is.null(v) ) {
        stop( paste( "feature", feature$name, "not found" ) );
      }
      else if ( feature$type == "CategoricalFeature" ) {
        if ( typeof(v) == "character" ) {
          categoryId <- feature$categoryIdByName[[ v ]];
          if ( is.null( categoryId ) ) {
            stop( paste("category", v, "not a valid one for feature", feature$name ) );
          }
          else {
            values <- append( values, categoryId );
          }
        }
        else {
          stop( paste("value for feature", feature$name, "not a string") );
        }
      }
      else if ( feature$type == "OrdinalFeature" ) {
        if ( typeof(v) == "double" ) {
          if ( checkOrdinalDomain ) {
            if ( !( feature$domainMin <= v && v <= feature$domainMax ) ) {
              stop( paste("value (", v, ") for feature", feature$name,
                          "is not within domain [", feature$domainMin, "," ,
                          feature$domainMax, "]" ) );
            }
            else {
              values <- append( values, v );
            }
          }
          else {
            values <- append( values, v );
          }
        }
        else {
          stop( paste("value for feature", feature$name, "not a double") );
        }
      }
      else {
        stop( paste("unknown feature type", feature$type ) );
      }
    }
    probability( predictTrees( model$trees, values ) );
  }
}

