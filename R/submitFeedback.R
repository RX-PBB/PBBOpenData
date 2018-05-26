#' A function to send feedback
#'
#' Sends email using mailR
#' @param name Sender's name
#' @param email Sender's email
#' @param comment Email's content, the feedback they submitted
#' @param emails vector of emails to send feedback to
#' @export
#' @examples
#' submitFeedback()
#'
submitFeedback<-function(name=input$name,email=input$email,comment=input$comment,emails=emails,Subject){

  body<-paste("<p><strong>",name,"</strong> has sent feedback from our PBB open data site.</p>
                        <p>",name," has provided the following email: ",email,"</p>
                        <p> Comments: </p>
                        <p>",comment," </p>",sep='')

        for(i in 1:length(emails)){
        send.mail(from = "helpdesk@resourcex.net",
                    to = emails[i],
                    subject = Subject,
                    body = body,
                    html=T,
                    smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "helpdesk@resourcex.net", passwd = "elonmusk2017", ssl = TRUE),
                    authenticate = T,
                    send = TRUE)
        }

        showModal(modalDialog(
            size='s',easyClose = T,
            h3('Thanks for your feedback!')
        ))

}
