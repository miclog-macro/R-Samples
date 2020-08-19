
library("pacman")
library("rsconnect")
library("dotenv")
rsconnect::setAccountInfo(
  name   = Sys.getenv('mlogalbomacro'),
  token  = Sys.getenv('D0DE6338FCC1281599B9CEC061B953CD'),
  secret = Sys.getenv('9bxDN/AhuMrxVL16ISo4dszdOz3akaUTKq27km5t')
)
rsconnect::deployApp(appName = 'dashboard')

