#Retrieve set of interest
#Example: All SNOMEDCT annotations for articles annotated with 
#"Renal cell carcinoma", 
#i.e, http://purl.bioontology.org/ontology/SNOMEDCT/41607009

library(dplyr)
if (!require("SPARQL")) {
  install.packages("SPARQL")
  library(SPARQL)
}

endpoint = "http://biotea.linkeddata.es/sparql"
query = "
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX oa:<http://www.w3.org/ns/oa#>
PREFIX biotea:<https://biotea.github.io/biotea-ontololgy#>

SELECT DISTINCT ?article ?annot STR(?strText) AS ?text xsd:integer(?tf) AS ?frequency ?ontoBody 
WHERE {
  SELECT DISTINCT ?article ?annot ?strText ?tf ?ontoBody 
  WHERE {
    ?annot a oa:Annotation ;
      oa:hasTarget ?paragraph ;
      oa:hasBody ?ontoBody ;
      oa:hasBody ?textualBody ;
      biotea:tf ?tf .
    ?paragraph oa:hasSource ?article .
    ?textualBody a oa:TextualBody ;
      rdf:value ?strText .
    FILTER ( STRSTARTS(STR(?ontoBody), \"http://purl.bioontology.org/ontology/SNOMEDCT\") )
    {
      SELECT DISTINCT ?article
      WHERE {
        ?annot a oa:Annotation ;
          oa:hasTarget ?paragraph ;
          oa:hasBody <http://purl.bioontology.org/ontology/SNOMEDCT/41607009> .
        ?paragraph oa:hasSource ?article .
      }
    }
  } ORDER BY ?article ?annot ?ontoBody
}"
queryLimit = as.integer(10000)
queryOffset = as.integer(0)
if (exists("resultset")) {
  rm(resultset)
}

repeat {
  q = paste(query, "LIMIT", queryLimit, "OFFSET", queryOffset, sep=" ")
  res = SPARQL(url=endpoint, q)$results
  resRows = nrow(res)
  print(resRows)
  if (resRows == 0) {
    print("Resultset ready")
    break;
  } else {
    if (!exists("resultset")) {
      resultset = res
    } else {
      resultset = rbind(resultset, res)
    }
    print(nrow(resultset))
    queryOffset = queryOffset + queryLimit
  }
}

View(resultset)

#Calculate the total number of SNOMEDCT annotations
if (!require("tidytext")) {
  install.packages("tidytext")
  library(tidytext)
}

totalAnnot = summarize(group_by(resultset, article), total = sum(frequency))

resultset = left_join(resultset, totalAnnot)

resultset = bind_tf_idf(resultset, ontoBody, article, frequency) 

#Calculate the cosine similarity between any two pair of articles
articles = unique(resultset[, "article"])
startPos = nchar("<http://linkingdata.io/pmcdoc/pmc/") + 1;
endPos = nchar(articles) - 1 
articleIds = substr(articles, startPos, endPos)
simMatrix = matrix(0, nrow = length(articles), 
 ncol = length(articles), dimnames = list(articleIds, articleIds))

for(i in 1:length(articles)) {
  docA <- resultset[resultset$article == articles[i],]
  for(j in i:length(articles)) {
    if (j == i) {
      simMatrix[i,j] = 1
    } else {
      docB <- resultset[resultset$article == articles[j],]
      common = merge(docA, docB, by = "ontoBody")
      commonA = abs(common[common$article.x == articles[i],'tf_idf.x'])
      commonB = abs(common[common$article.y == articles[j], 'tf_idf.y'])
      simMatrix[i,j] = (commonA %*% commonB) / (sqrt(commonA%*%commonA) * sqrt(commonB%*%commonB))
      simMatrix[j,i] = simMatrix[i,j]
    }
  }
}

#Plot the cosine similarity matrix as a heatmap
if (!require("gplots")) {
  install.packages("gplots")
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

cols <- brewer.pal(8,"Set3")
heatmap.2( 
  round(simMatrix, digits = 2), 
  Rowv=NA, Colv=NA, dendrogram = "none",
  trace="none",
  density.info = "none",
  col = colorRampPalette(brewer.pal(9,"Oranges"))(100),  margins=c(4,4)
)
