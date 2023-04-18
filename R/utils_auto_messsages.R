#' auto_messsages
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import blastula
#' @import lubridate
#' @importFrom glue glue
#' @aliases
#'
#' @noRd
email_push <- function(to,from,cc=NULL,message,Sender,Recipient=NULL,Subject,
                       attachments=NULL,credfile,test=T,sig = NULL){

    email <- compose_email(body = md(glue(

        'Good {ifelse(am(Sys.time())," morning "," afternoon ")} {Recipient},

{message}

Regards,

{Sender}'
    )),footer = md(c(if (!is.null(sig)) {add_image(sig,align = 'left') },"Email sent on: ",add_readable_time(use_tz=F),".")))
    # insert attacments
    if (!is.null(attachments)) {
        i = 1
        while (i <= length(attachments)) {
            email = email %>% add_attachment(file = attachments[[i]])
            i=i+1
        }
    }
    # send email
    if (test) {
        return(email)
    }else{
        creds = creds_file(file = credfile)
        email %>%
            smtp_send(to = to,
                      cc=cc,
                      from = from,
                      subject = Subject,
                      credentials = creds)
        # print(credfile)
    }
}


account_creation <- function(email,initial_password) {
    glue("

Welcome to maisha collection team.

Use the credentials below to log into your debt collection application.

Username: {email}

Password: {initial_password}
                       ")
}

pass_reset <- function(initial_password) {
    glue("

You requested us for a password reset.

Use the credentials below to log into your debt collection application in your next login.

Password: {initial_password}

")
}
