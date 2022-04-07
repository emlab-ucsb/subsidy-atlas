### -----------------------------------
#
# ACP Atlas of Distant Water Fishing
# UI - Tab 11: Data and Assumptions
# 
### ------------------------------------

### Function
Assumptions = function(){
  
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Page Header
    column(12, id = "tblr-small-spaced-div",
           
           # Title
           tags$h4("Data & Methods")
           
    ),
    
    ### Intro and purpose
    column(12, id = "tblr-small-spaced-div",
           
           # Text
           includeHTML("./text/faqs-and-contact/intro_and_purpose.html")
           
    ),
    
    ### Methods download 
    column(12, id = "tblr-small-spaced-div",
           
           # File download buttons
           column(12, style = "text-align: center;",

                  fluidRow(

                    downloadButton("db_methods",
                                   "Methodology\n(PDF)")
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
               width = 12),
           
           # four
           box(title = "Where did the land and ocean area boundaries depicted in this tool come from?",
               includeHTML("./text/faqs-and-contact/faq_four.html"),
               collapsible = T,
               collapsed = T,
               width = 12),
           
    ),
    
    ### Assumptions
    column(12, id = "tblr-small-spaced-div",
           
           # Text
           includeHTML("./text/faqs-and-contact/assumptions.html")
           
    )
    
  ) # /fluidPage
}
