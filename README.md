# eCRF-SMCcath
eCRF for SMC Cath lab


<!-- how to edit module  -->

<!-- 0. modify cars_table_module.R -->
  <!-- ids.na.adm ~~ -->

<!-- 1. build module.R -->
  <!-- adm_edit_module.R -->
<!-- 2. edit db_init.R -->
  <!-- 2-1. describe db schema-->
  <!-- 2-2. insert example data -->
  <!-- 2-3. run db_init.R -->

<!-- 3. edit ~~_edit_module.R -->
  <!-- 3-1. edit edit_car_dat -->

<!-- 4. attatch module.R --> 
  <!--  car_to_edit_m3 <- eventReactive(input$car_id_to_edit_m3, { -->
  <!--  # Set the Action Buttons row to the first column of the `mtcars` table -->
  <!--      escape = -which(names(out) %in% c(" ", "Demographics", "Admission", "Outcomes", "M1", "M3", "M6", "Mf")), -->
  <!--           list(targets = which(names(out) %in% c(" ", "Demographics", "Events", "Labs", "M1", "M3")) - 1, orderable = FALSE), -->

<!-- 5. modify javascript -->

<!-- for references -->

<!--
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
-->