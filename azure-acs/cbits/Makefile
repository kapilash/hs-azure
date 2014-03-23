

trial.out : trial.c AzureAcs.o
	clang trial.c AzureAcs.o -lcurl -o trial.out


AzureAcs.o : AzureAcs.c AzureAcs.h AzureAcsInternal.h
	clang -c AzureAcs.c

simple.out : simple.c
	clang  simple.c -o simple.out -lcurl

clean :
	rm trial.out AzureAcs.o simple.out
