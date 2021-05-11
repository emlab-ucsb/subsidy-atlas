### -----------------------------------
#
# ACP Atlas of Distant Water Fishing
# UI - Tab 9: FAQs & Contact
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
           tags$h4("More Info, FAQs, and Contact")
           
    ),
    
    ### Intro and purpose
    column(12, id = "tblr-small-spaced-div",
           
           # Text
           includeHTML("./text/faqs-and-contact/intro_and_purpose.html")
           
    ),
    
    ### Data summaries
    column(12, id = "tblr-small-spaced-div",
           
           # Text
           includeHTML("./text/faqs-and-contact/summary.html"),
           
           # File download buttons
           column(12, style = "text-align: center;", id = "t-spaced-div",
                  
                  fluidRow(
                    
                    downloadButton("db_data_summary_by_eez", 
                                   "Summary by EEZ (CSV)"),
                    
                    downloadButton("db_data_summary_by_region", 
                                   "Summary by High Seas Region (CSV)"),
                    
                    downloadButton("db_data_summary_by_flag", 
                                   "Summary by Flag State (CSV)")
                  )
                  
           )

    ),
    
    ### FAQs
    column(12, id = "tblr-small-spaced-div",
           
           # Text
           includeHTML("./text/faqs-and-contact/faqs.html"),
           
           # one
           box(title = "Where did the vessel data used in this tool come from?",
               includeHTML("./text/faqs-and-contact/faq_one.html"),
               collapsible = T,
               collapsed = T,
               width = 12),
           
           # two
           box(title = "Where did the fisheries subsidies data used in this tool come from?",
               includeHTML("./text/faqs-and-contact/faq_two.html"),
               collapsible = T,
               collapsed = T,
               width = 12),
           
           # three
           box(title = "How are subsidies allocated?",
               includeHTML("./text/faqs-and-contact/faq_three.html"),
               collapsible = T,
               collapsed = T,
               width = 12)
           
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
