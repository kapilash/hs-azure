#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "AzureAcs.h"

int main(int argc, char **argv)
{
  azureACS_context context;
  azureACS_info info = (azureACS_info)calloc(1,sizeof(struct azureACS_info_str));
  azureACS_token token;
  char *tokenStr;
  long seconds;
  int tokenStatus;

  info->relyingParty = "http://{{NS}}.servicebus.windows.net/";
  info->key = "{{ISSUER-KEY}}";
  info->issuerName = "owner";
  info->acsUrl = "https://{{NS}}-sb.accesscontrol.windows.net/WRAPv0.9/";

  azureACS_initialize();
  context = azureACS_getContext(info);

  if(!azureACS_isValid(context))
    azureACS_printError(context);
  token = azureACS_getToken(context);

  tokenStatus = azureACS_tokenStatus(token);
  if(tokenStatus != 200)
    fprintf(stderr,"status = %d,Error = %s\n",tokenStatus,azureACS_tokenError(token));


  printf("Token = %s\n",azureACS_wrapAccessToken(token));
  printf("seconds = %ld\n ", azureACS_validForSeconds(token));


   azureACS_finalizeContext(context);
   azureACS_finalizeToken(token);


  return 0;
}
