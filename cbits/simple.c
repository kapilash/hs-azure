#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <curl/curl.h>
#include <fcntl.h>
#include <sys/stat.h>
#define BUFLEN 2000

struct Acs_Response{
  CURL *curl;
  char *token;
  int length;
};

size_t reader(void *ptr,size_t size, size_t nmemb, void *userdata)
{
  int i=0,length = 0, toContinue = 0;
  size_t max = size*nmemb;
  char *queuedesc = (char *)userdata;
  length = strlen(queuedesc);

  printf("\nread(%zu,%zu,%d)\n",size,nmemb,length);
  memcpy(ptr,queuedesc,length);
    return length;
}
static size_t read_callback(void *ptr, size_t size, size_t nmemb, void *stream)
{
  size_t retcode;
  curl_off_t nread;
 
  /* in real-world cases, this would probably get this data differently
     as this fread() stuff is exactly what the library already would do
     by default internally */ 
  retcode = fread(ptr, size, nmemb, stream);
 
  nread = (curl_off_t)retcode;
 
  fprintf(stderr, "*** We read %" CURL_FORMAT_CURL_OFF_T
          " bytes from file\n", nread);
 
  return retcode;
}
size_t writer( char *ptr, size_t size, size_t nmemb, void *userdata)
{
  int i=0,begin,tokenlen,outlen = 0;
  size_t bytelen = size*nmemb;
  char *buf;
  struct Acs_Response *response = (struct Acs_Response *)userdata;
  printf("\n(%zu,%zu)\n",size,nmemb);
  i = 0;
  for(i=0;ptr[i]!= '=';i++);
  i++;
  begin = i;
  tokenlen = 0;
  while((ptr[i] != '&') && (i < bytelen)){
    i++;tokenlen++;
  }
  printf("\n");
  response->token = curl_easy_unescape(response->curl,&(ptr[begin]),tokenlen,&outlen);
  response->length = outlen;
  return bytelen;
}

int appendToBuffer(char *buffer, int position, const char *str)
{
  int index,pos;
  int len = strlen(str);
  pos = position;
  for(index = 0;pos < BUFLEN; index++,pos++){
    if(str[index] == '\0')
      break;
    buffer[position+index] = str[index];
  }
  return pos;  
}

void createBuffer(CURL *curl,char *buffer,const char *relyingPartyUrl,const char *issuer,const char *password)
{
  int index = 0;
  char *escapedUrl;
  char *escapedPass;
  escapedUrl = curl_easy_escape(curl,relyingPartyUrl,0);
  escapedPass = curl_easy_escape(curl,password,0);

		
  for(index = 0; index < BUFLEN; index++) {
    buffer[index] = '\0';
  }
  index = 0;
  index = appendToBuffer(buffer,index,"wrap_scope=");

  index = appendToBuffer(buffer,index,escapedUrl);

  index = appendToBuffer(buffer,index,"&wrap_name=");

  index = appendToBuffer(buffer,index,issuer);

  index = appendToBuffer(buffer,index,"&wrap_password=");

  index = appendToBuffer(buffer,index,escapedPass);

  curl_free(escapedPass);
  curl_free(escapedUrl);
}

void createQueue(char *token,int tokenLen)
{
  CURL *curl;
  CURLcode res;
  FILE *fp;
  static const char buf[] = "Content-Type: text/plain";
  char *errorBuffer  = (char *)malloc(sizeof(char) * CURL_ERROR_SIZE);
  char *authToken = (char *)malloc(sizeof(char) *(tokenLen + 50));
  const char queueData[] = "Hello world!";
  int buflen = BUFLEN,authIndex,bufIndex;
  struct curl_slist *headerlist=NULL;
  struct Acs_Response req;
  struct stat file_info;

  stat("QueueDesc",&file_info);
  fp = fopen("QueueDesc","rb");

  memset(authToken,0,tokenLen+50);
  authIndex = 0;
  authIndex = appendToBuffer(authToken,authIndex,"Authorization: WRAP access_token=\"");
  authIndex = appendToBuffer(authToken,authIndex,token);
  appendToBuffer(authToken,authIndex,"\"");

  headerlist = curl_slist_append(headerlist,authToken);
  headerlist = curl_slist_append(headerlist,buf);
  curl = curl_easy_init();  
  if(curl){
    req.curl = curl;
    curl_easy_setopt(curl,CURLOPT_URL,"https://{{NS}}.servicebus.windows.net/kqueue/Messages");
    //curl_easy_setopt(curl, CURLOPT_UPLOAD, 1L);
    curl_easy_setopt(curl,CURLOPT_HTTPHEADER,headerlist);
    curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errorBuffer);
    curl_easy_setopt(curl,CURLOPT_VERBOSE,1);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, queueData);
    //    curl_easy_setopt(curl, CURLOPT_READFUNCTION, read_callback);
    // curl_easy_setopt(curl,CURLOPT_INFILESIZE,bufIndex);
    //curl_easy_setopt(curl,CURLOPT_READDATA,fp);
    //curl_easy_setopt(curl, CURLOPT_INFILESIZE_LARGE,
    //               (curl_off_t)file_info.st_size);
    res = curl_easy_perform(curl);
    if(res != CURLE_OK){
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
	      curl_easy_strerror(res));
      fprintf(stderr,"%s",errorBuffer);
    }else{
      printf("\n");
    }
    curl_easy_cleanup(curl);
    //curl_formfree(formpost);
    curl_slist_free_all(headerlist);
    free(authToken);

    free(errorBuffer);

  fclose(fp);
  }

}

int main(void)
{
  CURL *curl;
  CURLcode res;
  struct curl_httppost *formpost=NULL;
  struct curl_httppost *lastptr=NULL;
  struct curl_slist *headerlist=NULL;
  static const char buf[] = "Content-Type: application/x-www-form-urlencoded";
  char *errorBuffer  = (char *)malloc(sizeof(char) * CURL_ERROR_SIZE);
  char *buffer = (char *)malloc(sizeof(char) * BUFLEN);
  int index,buflen = BUFLEN;
  struct Acs_Response response; 

  createBuffer(curl,buffer,"http://{{NS}}.servicebus.windows.net/","owner","{{ISSUER-KEY}}");
  printf("BUFFER = %s and len = %ld",buffer,strlen(buffer));
  curl_global_init(CURL_GLOBAL_DEFAULT);
  
  curl = curl_easy_init();
  headerlist = curl_slist_append(headerlist,buf);
  if(curl){
    response.curl = curl;
    curl_easy_setopt(curl,CURLOPT_URL,"https://{{NS}}-sb.accesscontrol.windows.net/WRAPv0.9/");
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, buffer);
    curl_easy_setopt(curl,CURLOPT_HTTPHEADER,headerlist);
    curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errorBuffer);
    curl_easy_setopt(curl,CURLOPT_VERBOSE,1);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writer);
    curl_easy_setopt(curl,CURLOPT_WRITEDATA,&response);

    res = curl_easy_perform(curl);
    if(res != CURLE_OK){
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
	      curl_easy_strerror(res));
      fprintf(stderr,"%s",errorBuffer);
    }else{
      index = 0;
      for(index =0; index < response.length; index++)
	printf("%c",response.token[index]);
      printf("\n");
    }
    curl_easy_cleanup(curl);

    //curl_formfree(formpost);
    curl_slist_free_all(headerlist);
    free(buffer);
    free(errorBuffer);
    curl_free(response.token);
  }
  return 0;
}
