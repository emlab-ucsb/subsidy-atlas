### -----------------------------------
#
# ACP Atlas of Distant Water Fishing
# UI - Tab 12: FAQs & Contact
# 
### ------------------------------------

### Function
Contact = function(){
  
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Page Header
    column(12, id = "tblr-small-spaced-div",
           
           # Title
           tags$h4("Glossary & Contact")
           
    ),
    
    ### Glossary
    column(12, id = "tblr-small-spaced-div",
           
           # Text
           includeHTML("./text/faqs-and-contact/glossary.html")
           
    ),
    
    ### Contact us
    column(12, id = "tblr-small-spaced-div",
           
           # More text
           includeHTML("./text/faqs-and-contact/contact_us.html")
           
    ),
    
    ### Logos
    column(12, id = "tblr-small-spaced-div", align = "center", style = "padding-bottom: 40px;",
           
           tags$table(id = "methods-process-table",
                      
                      tags$tr(id = "methods-process-table-row",
                              
                              tags$td(id = "methods-process-table-cell-1",
                                      
                                      tags$image(src = "pew_logo.jpg",
                                                 style = "height: 60px;")
                                      
                              ),
                              
                              tags$td(id = "methods-process-table-cell-2",
                                      
                                      tags$image(src = "ubc_logo.png",
                                                 style = "height: 60px;")
                                      
                              ),
                              
                              tags$td(id = "methods-process-table-cell-3",
                                      
                                      tags$image(src = "gfw_logo.png",
                                                 style = "height: 60px;")
                                      
                              )
                      )
           )
    )
    
  ) # /fluidPage
}
