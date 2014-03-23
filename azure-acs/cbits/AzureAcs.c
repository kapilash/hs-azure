#include "AzureAcsInternal.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define AZURE_ACS_STR_MEM(x) ((unsigned char *)calloc(( 1 + (x)),(sizeof(unsigned char))))
#define AZURE_ACS_STR_CPY(dest,src,len) memcpy((dest),(src),((len)+1))


static size_t headerWriter( char *ptr, size_t size, size_t nmemb, void *userdata)
{
  int i=0;
  int status = 0;
  size_t bytelen = size*nmemb;
  TokenData response = (TokenData)userdata;

  if(bytelen > 9){
    if(strncmp(ptr,"HTTP/1.1 ",9) == 0){
      for(i=9; (i<bytelen) && (ptr[i] != ' ') ; i++){
	status = (status * 10) + (ptr[i] - '0');
      }
      response->status = status;
      response->error = (char *)calloc(bytelen+1,sizeof(char));
      for(i=0;i<bytelen;i++)
	response->error[i] = ptr[i];
    }
  }
  return bytelen;
}


static size_t writer( char *ptr, size_t size, size_t nmemb, void *userdata)
{
  int i=0,begin,tokenlen,outlen = 0;
  size_t bytelen = size*nmemb;
  TokenData response = (TokenData)userdata;
  i = 0;
  long seconds = 0;
  for(i=0;ptr[i]!= '=';i++);
  i++;
  begin = i;
  tokenlen = 0;
  while((ptr[i] != '&') && (i < bytelen)){
    i++;tokenlen++;
  }
  printf("\n");

  response->token = curl_easy_unescape(NULL,&(ptr[begin]),tokenlen,&outlen);
  for(;ptr[i] != '=';i++);
  i++;
  
  for(;i<bytelen;i++){
    seconds = (seconds * 10) + (ptr[i] - '0');
  }
  response->length = outlen;
  response->seconds = seconds;
  
  return bytelen;
}


static int appendToBuffer(char *buffer,int bufferLen, int position, const char *str)
{
  int index,pos;
  int len = strlen(str);
  pos = position;
  for(index = 0;pos < bufferLen; index++,pos++){
    if(str[index] == '\0')
      break;
    buffer[position+index] = str[index];
  }
  return pos;  
}

static void createBuffer(char *buffer,int bufferLen,azureACS_info acsInfo)
{
  int index = 0;
  char *escapedUrl;
  char *escapedPass;
  escapedUrl = curl_easy_escape(NULL,acsInfo->relyingParty,0);
  escapedPass = curl_easy_escape(NULL,acsInfo->key,0);
  index = 0;

  index = appendToBuffer(buffer,bufferLen,index,"wrap_scope=");

  index = appendToBuffer(buffer,bufferLen,index,escapedUrl);

  index = appendToBuffer(buffer,bufferLen,index,"&wrap_name=");

  index = appendToBuffer(buffer,bufferLen,index,acsInfo->issuerName);

  index = appendToBuffer(buffer,bufferLen,index,"&wrap_password=");

  index = appendToBuffer(buffer,bufferLen,index,escapedPass);
  curl_free(escapedPass);
  curl_free(escapedUrl);
}

void azureACS_initialize()
{
  curl_global_init(CURL_GLOBAL_DEFAULT);
}

azureACS_context azureACS_getContext(azureACS_info acsInfo){
  azureACS_credentials credentials = NULL;
  CURL *curl;
  static const char buf[] = "Content-Type: application/x-www-form-urlencoded";
  char *errorBuffer  = (char *)calloc(CURL_ERROR_SIZE,sizeof(char));
  char *buffer;
  struct curl_slist *headerlist=NULL;
  int index,buflen;
  TokenData response;
  int rpLen,inLen,keyLen;

  rpLen  = strlen(acsInfo->relyingParty);
  inLen = strlen(acsInfo->issuerName);
  keyLen = strlen(acsInfo->key);

  buflen = 3*(rpLen + inLen + keyLen); 
  buffer = (char *)calloc(buflen,sizeof(char));


  createBuffer(buffer,buflen,acsInfo);

  credentials = (azureACS_credentials)calloc(1,sizeof(struct azureACS_credentials_str));
  response = (TokenData)calloc(1,sizeof(struct azureACS_token_str));

  credentials->relyingParty = AZURE_ACS_STR_MEM(rpLen);
  credentials->issuerName   = AZURE_ACS_STR_MEM(inLen);
  credentials->key          = AZURE_ACS_STR_MEM(keyLen);

  credentials->errorBuffer = errorBuffer;

  AZURE_ACS_STR_CPY(credentials->relyingParty,acsInfo->relyingParty,rpLen);
  AZURE_ACS_STR_CPY(credentials->issuerName,acsInfo->issuerName,inLen);
  AZURE_ACS_STR_CPY(credentials->key,acsInfo->key, keyLen);

  credentials->headerList = curl_slist_append(credentials->headerList,buf);
  curl = curl_easy_init();
  if(curl){
    curl_easy_setopt(curl,CURLOPT_URL,acsInfo->acsUrl);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, buffer);
    curl_easy_setopt(curl,CURLOPT_HTTPHEADER,credentials->headerList);
    curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errorBuffer);
    curl_easy_setopt(curl,CURLOPT_VERBOSE,0);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writer);
    curl_easy_setopt(curl,CURLOPT_WRITEDATA,response);
    curl_easy_setopt(curl,CURLOPT_WRITEHEADER,response);
    curl_easy_setopt(curl,CURLOPT_HEADERFUNCTION,headerWriter);
    credentials->response = response;
    credentials->curl = curl;
    credentials->buffer = buffer;
    credentials->isValid = 1;
  }else{
    credentials->isValid = 0;
    credentials->curl = NULL;
  }
  return credentials;
}

void azureACS_finalizeContext(azureACS_context context){
  azureACS_credentials credentials = (azureACS_credentials)context;
  free(credentials->relyingParty);
  free(credentials->key);
  free(credentials->issuerName);
  curl_slist_free_all(credentials->headerList);
  curl_easy_cleanup(credentials->curl);
  free(credentials->buffer);
  free(credentials);
}

azureACS_token azureACS_getToken(azureACS_context context){
  CURLcode res;
  azureACS_credentials creds = (azureACS_credentials)context;
  res = curl_easy_perform(creds->curl);

  if(res != CURLE_OK){
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
	    curl_easy_strerror(res));
    fprintf(stderr,"%s",creds->errorBuffer);
    creds->isValid = 0;
  }
  return creds->response;
}

void azureACS_finalizeToken(azureACS_token token){
  TokenData tokenData = (TokenData)token;

  curl_free(tokenData->token);
  if(tokenData->error != NULL)
    free(tokenData->error);
  free(tokenData);
}

const char *azureACS_wrapAccessToken(azureACS_token token){
  TokenData tokenData = (TokenData)token;
  return tokenData->token;
}
long azureACS_validForSeconds(azureACS_token token){
  TokenData tokenData = (TokenData)token;
  return tokenData->seconds;
}
int azureACS_tokenStatus(azureACS_token token){
  TokenData tokenData = (TokenData)token;
  return tokenData->status;
}
void azureACS_printError(azureACS_context context){
  azureACS_credentials creds= (azureACS_credentials)context;
  fprintf(stderr,"%s",creds->errorBuffer);
long seconds = 0;}

int azureACS_isValid(azureACS_context context){
  azureACS_credentials creds = (azureACS_credentials)context;
  return creds->isValid;
}
char *azureACS_tokenError(azureACS_token token){
  return ((TokenData)token)->error;
}
