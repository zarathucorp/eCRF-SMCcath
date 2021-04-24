# eCRF-SMCcath
eCRF for SMC Cath lab


<!-- how to edit module  -->

<!-- 1. build module.R -->

<!-- 2. edit db_init.R -->
  <!-- 2-1. describe db schema-->
  <!-- 2-2. insert example data -->
  <!-- 2-3. run db_init.R -->

<!-- 3. edit module.R -->
  <!-- 3-1. edit edit_car_dat -->
  

<!-- for references -->

```R
myf <- function(v, type){
  if(type!='DATE'){print(paste0("'", v, "' = ifelse(is.null(input$", v, "), '', input$", v, '),' ))}
  if(type=='DATE'){print(paste0("'", v, "' = ifelse(is.null(input$", v, "), '', as.character(input$", v,')),'))}
}

# Example
myf('Cardiac_Status_M3', 'TEXT')
myf('Visit_Date_M3', 'DATE')
myf("SBP_M3", 'REAL')
```
  
<!-- 4. edit javascript code -->

