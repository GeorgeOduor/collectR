library(RSelenium)
library(wdman)
# selServ <- selenium(verbose = FALSE, check = FALSE)
cDrv <- chrome(verbose = FALSE, check = T,port = 4444L,version = '112.0.5615.49')

remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4444L,
    browserName = "phantomjs"
)
remDr$open()
remDr$errorDetails()

remDr$navigate("https://web.whatsapp.com/")

element <- remDr$findElement(using = "xpath",value = '//*[@id="pane-side"]/div/div/div/div[1]')
# element <- remDr$findElement(using = "xpath",value = '//*[@id="pane-side"]/div/div/div/div[11]')

element$clickElement()
contact <- element$findChildElements('xpath','//*[@id="main"]/header/div[2]/div/div/span')
contact <- contact[[1]]$getElementText()[[1]]
contact
conversation <- remDr$findElements(using = 'class',value = 'hY_ET')


for (msg in conversation) {
    checkidisplayed <- msg$isElementDisplayed()
    # scrollbutton <- msg$findElement('xpath','//*[@id="main"]/div[2]/div/div[1]/span/div/span[2]')
    msg_container <- msg$findElement('class','_1-lf9')
    message <- msg_container$findElement('tag','span')
    # outmessage$getElementText() %>% print()
    message_sent <- message$findElement('class','_21Ahp')$findElement('class','_11JPr')$getElementText() %>% unlist()
    message_sent <- message$findElement('class','copyable-text')
    # sent_time <- strsplit(message_sent[[1]],"\n") %>% tail(1)
    issent = str_detect(message$getElementAttribute(attrName = 'data-testid') ,"true")
    print(issent)
    print(message_sent$getElementText() %>% unlist())

    # get sent messages
}


