#include <stdio.h>
#include <stdlib.h>
#include <time.h>


#define POSITIVE 1;
#define NEGATIVE -1;

#define MAX 10000
#define str_max 1000
#define max_max 8*1000
#define max_max_max 17
#define MOD 1000000000
#define VALUETYPE long

#define EVEN(x) ((x&1)==0)
#define ODD(x) ((x&1)==1)

long long int cipher_text[8000];

typedef struct {
    long long int numerator;
    long long int denominator;
    int sign; // true indicate positive, false indicate negative
    int exponent; // only used for property 2
} fraction;

fraction prop1(fraction);
fraction prop2(fraction);
fraction prop3(fraction);
fraction prop4(fraction);
long long int jacobi(long long int a, long long int b);
long long int fractionToInt(fraction);
long long int isEven(long long int);
void printFraction(fraction);

long long int my_strlen(char *string)
{
    long long int length;
    for (length = 0; *string != '\0'; string++)
    {
        length++;
    }
    return(length);
}


//  Compute x^n mod y in a quick way.
VALUETYPE PowerMod(VALUETYPE x, VALUETYPE y, VALUETYPE n) {
  if (n == 0) 
    return 1;
  if (n == 1) 
    return x;
  if EVEN(n) 
    return PowerMod(x*x%y, y, n/2) % y;
  return PowerMod(x*x%y, y, n/2) * x % y;
}

//  If n is a prime
int Miller_Rabin(VALUETYPE n) 
{
    if (n == 2) 
        return 1;
    if ((n<2)||EVEN(n)) return 0;
        VALUETYPE d;
    d = n-1;
    while EVEN(d) d = d >> 1;
    VALUETYPE base[7] = { 2, 3, 5, 7, 11, 13, 17};
    int td, t,i;
    for ( i=0; i<7; i++) 
    {
        if (base[i] >= n) 
            break;
        td = d;
        t = PowerMod( base[i], n, td);
        while ((td!=n-1)&&(t!=1)&&(t!=n-1)) 
        {
            t = t*t % n;
            td = td << 1;
        }
        if ((t == n-1)||ODD(td)) ; else return 0;
    }

    return 1;

}

long int return_prime()
{
    long int x;
    int b;
//    srand(time(NULL));
    while(1)
    {
        x=rand() % MOD;

        b=Miller_Rabin(x);  

        if(b==1)
            break;
    }
    //printf("%ld\n",x);
    return x;
} 


long long powi(long long base, long long ex){
    // power of 0
    if (ex == 0)
    {
        return 1;
    // negative exponenet
    }
    else if( ex < 0)
    {
        return 1 / powi(base, -ex);
    // even exponenet
    }
    else if ((int)ex % 2 == 0)
    {
        long half_pow = powi(base, ex/2);
        return half_pow * half_pow;
    //integer exponenet
    }
    else
    {
        return base * powi(base, ex - 1);
    }
}

char * multiply(char a[],char b[])
{
    static char mul[MAX];
    char c[MAX];
    char temp[MAX];
    int la,lb;
    int i,j,k=0,x=0,y;
    long int r=0;
    long sum = 0;
    la=my_strlen(a)-1;
        lb=my_strlen(b)-1;
   
        for(i=0;i<=la;i++)
        {
                a[i] = a[i] - 48;
        }

        for(i=0;i<=lb;i++)
        {
                b[i] = b[i] - 48;
        }

    for(i=lb;i>=0;i--)
    {
         r=0;
         for(j=la;j>=0;j--)
         {
             temp[k++] = (b[i]*a[j] + r)%10;
             r = (b[i]*a[j]+r)/10;
         }
         temp[k++] = r;
         x++;
         for(y = 0;y<x;y++)
         {
             temp[k++] = 0;
         }
    }
   
    k=0;
    r=0;
    for(i=0;i<la+lb+2;i++)
    {
        sum =0;
        y=0;
        for(j=1;j<=lb+1;j++)
        {
            if(i <= la+j)
            {
                sum = sum + temp[y+i];
            }
             
            y += j + la + 1;
        }
         
        c[k++] = (sum+r) %10;
        r = (sum+r)/10;
    }
    
    c[k] = r;
    j=0;
    for(i=k-1;i>=0;i--)
    {
         mul[j++]=c[i] + 48;
    }
    
    mul[j]='\0';
    
    return mul;
}


long long int modulus(char *c, long long int p)
{
    long long int mod,i;
    mod=(c[0]-'0')%p;
    //printf("%d\n",mod*10);
    for(i=1;c[i]!='\0';i++)
    {
        //printf("In For %c\n",c[i]);
        mod=((mod*10)+(c[i]-'0'))%p;
      //  printf("Mod %d\n",mod);
    }
    return mod;
}

// returns x where (a * x) % b == 1
long long int mul_inv(long long int a, long long int b)
{
    long long int b0 = b, t, q;
    long long int x0 = 0, x1 = 1;
    if (b == 1) return 1;
    while (a > 1) 
    {
        q = a / b;
        t = b, b = a % b, a = t;
        t = x0, x0 = x1 - q * x0, x1 = t;
    }
    
    if (x1 < 0) x1 += b0;
    //printf("Modular Inverse %lld\n",x1);
    return x1;
}
 
long long int chinese_remainder(long long int *n, long long int *a, long long int len)
{
    long long int p, i, prod = 1, sum = 0,x1;
    char *mid_result,t1[20],t2[20],t3[20],*res;
    for (i = 0; i < len; i++) prod *= n[i];
 
    for (i = 0; i < len; i++) 
    {
        p = prod / n[i];
  //      printf("In for of chinese_remainder\n" );
        sprintf(t1,"%lld",a[i]);
  //      printf("t1 %s\n",t1);

        sprintf(t2,"%lld",p);
  //      printf("t2 %s\n",t2);
        
        sprintf(t3,"%lld",mul_inv(p,n[i]));
  //      printf("t3 %s\n",t3);
        
        
//        printf("t3 %s\n",t3);
        res=multiply(t1,t2);

        x1=modulus(res,prod);
        sprintf(t1,"%lld",x1);

        res=multiply(t1,t3);
        x1=modulus(res,prod);

        sum = (sum + x1)%prod;
    }
 
    return sum % prod;
}


long long int myAtoi(char a[]) 
{
    long long int c, sign, offset, n;
 
    if (a[0] == '-') 
    {  // Handle negative integers
        sign = -1;
    }
 
    if (sign == -1) 
    {  // Set starting position to convert
        offset = 1;
    }
    else 
    {
        offset = 0;
    }
 
    n = 0;
 
    for (c = offset; a[c] != '\0'; c++) 
    {
        n = n * 10 + a[c] - '0';
    }
 
    if (sign == -1) 
    {
        n = -n;
    }
 
    return n;
}



char *encrypt(char b_message[],long long int n, long long int x,long long int prime_p)
{
    //First Choose ai's Randomly for each mi's
    long long int len,i,a,cipher_i,x_power;
    int j,result_c;
    static char cipher[max_max_max];
    char x_power_j[18],a_i_square[15];
    char *res;

    fraction input1;
    input1.sign = POSITIVE;
    srand(time(NULL));

    len=my_strlen(b_message);
    //printf("Message in Binary\n");
    for (i = 0; i < len; ++i)
    {
        j=b_message[i]-'0';
        /* code */
        a=rand() % 10000000;
        //printf("j=%d\n",j);
        x_power=powi(x,j);

        sprintf(x_power_j,"%lld",x_power);
        sprintf(a_i_square,"%lld",a*a);

        //printf("%s\n",x_power_j);
        res=multiply(x_power_j,a_i_square);

        //printf("%s\n",res);
        cipher_i=modulus(res,n);

        //sprintf(cipher[i],"%lld",cipher_i);
        cipher_text[i]=cipher_i;
//        printf("%lld",cipher_i);


    }
  //  printf("\n");
    return cipher;
}

long int binary_decimal(long int n) /* Function to convert binary to decimal.*/

{
    long int decimal=0, i=0, rem;
    while (n!=0)
    {
        rem = n%10;
        n/=10;
        decimal += rem*powi(2,i);
        ++i;
    }
    return decimal;
}


void decrypt(long long int cipher_enc[],long long int prime_p,long long int l)
{
    long long int i,m;

    int result,n;
    char bin_message[max_max],c,basic[8];
    long int num;
    printf("\n\nDecrypted Message in Binary\n");

    for(i=0;i<l;i++)
    {
        result=jacobi(cipher_text[i],prime_p);

        if(result==1)
        {
            bin_message[i]='0';
        }
        else
        {
            bin_message[i]='1';
        }
    }
    printf("%s\n",bin_message);
    
    printf("\nDecrypting Message\n");

    m=0;
    while(m<l)
    {   
        //printf("h");
        for (i = 0; i < 8; i++)
        {
            
            basic[i]=bin_message[m];
    //        printf("basic[%lld]=%c",i,basic[i]);
            m++;
        }
      //  printf("\n");
//        printf("%s",basic);    

        num=myAtoi(basic);
        //printf("%ld\n",num );
        n=binary_decimal(num);
        //printf("  %d \n",n);
        c=(char)n;

        printf("%c",c);
        
    }
    printf("\n\n");
    
}


int main() 
{

    fraction input;

    long long int prime_p,prime_q,p_mul_q,x_mod_n,l;
    long random_num_a,random_num_b,m;

    int result_p,result_q,i,j,random_num_p,random_num_q,bin_digit;
    input.sign = POSITIVE;

    char temp[20];
    char message[str_max];
    char bin_message[max_max];
    char *cipher;
/*    char primes[33][10]={       "630045137","202551667",
                                "436273009","253878403",
                                "649580171","215949407",
                                "944192807","192983851",
                                "387096133","232423823",
                                "191912783","112098817",
                                "555142061","107534587",
                                "693103639","378043979",
                                "367876529","166726367",
                                "391995431","123454691",
                                "673919143","142414669",
                                "216668603","134065829",
                                "189695659","147684137",
                                "525436489",
                                "607010093",
                                "895858039",
                                "519653371",
                                "409866323",
                                "122164747",
                                "327966101"
                            };
*/
    srand(time(NULL));
    
    xy:

    random_num_p=rand() % 33;
    // Generating Index for Prime p
    random_num_q=rand() % 33;
    // Generating Index for Prime q

    if(random_num_p==random_num_q)
        goto xy;



//    prime_p=myAtoi(primes[random_num_p]);
    //Saving prime p in prime_p
    prime_p=return_prime();    
    
//    prime_q=myAtoi(primes[random_num_q]);
    //Saving prime q in prime_q
    prime_q=return_prime();  
    
    //while loop is for generating 'a' for jacobian (a|p)
    do
    {

        random_num_a = rand()%1000000;
        // random number a loop will go on till (a|p) becomes -1

        // passing random number a and prime p to jacobi function  
        result_p=jacobi(random_num_a,prime_p);
        
    }while(result_p!=-1);

    
    // we have a as random_num_a;

    // while loop is for generating 'b' for jacobian (b|q) becomes -1
    do
    {

        random_num_b = rand()%1000000;

        // passing random number b and prime no q to jacobi function
        
        result_q=jacobi(random_num_b,prime_q);
        
    }while(result_q!=-1);

    printf("random_num_a %ld\n",random_num_a);
    printf("random_num_b %ld\n",random_num_b);
    printf("prime_p %lld\n",prime_p);
    printf("prime_q %lld\n",prime_q);

    long long int n[] = { prime_p, prime_q };
    long long int a[] = { random_num_a, random_num_b };

    x_mod_n=chinese_remainder(n, a, sizeof(n)/sizeof(n[0]));
    p_mul_q=prime_p*prime_q;

    printf("Public Key (%lld,%lld)\n",p_mul_q,x_mod_n);
    
    printf("Enter the String/Message to encrypt\n");

    scanf("%s",message);
    
    m=0;
    
    for(i=0;message[i]!='\0';i++)
    {
        //printf("%c=",message[i]);
        for(j=0;j<8;j++)
        {
             bin_digit=(!!((message[i] << j) & 0x80));
             printf("%d",bin_digit);
             bin_message[m]=(char)bin_digit+48;
             //printf("\t%c",bin_message[m] );
             m++;
             //printf("%ld", m);
        }
        printf("\n");
    }

    printf("Message in Binary %s\n", bin_message);

    encrypt(bin_message,p_mul_q,x_mod_n,prime_p);

    printf("Encrypted Text\n");

    l=my_strlen(bin_message);

    for(i=0;i<l;i++)
    {
        printf("%lld",cipher_text[i]);
    }
    printf("\n");

    decrypt(cipher_text,prime_p,l);

    return 0;
}

long long int jacobi(long long int a, long long int n) 
{
     int ans;

    if (a == 0)
    {
        ans = (n == 1) ? 1 : 0;
    }
    
    else if (a == 2) 
    {
        switch ( n % 8 ) 
        {
            case 1:
            case 7:
                    ans = 1;
                    break;
            case 3:
            case 5:
                    ans = -1;
                    break;
        }
    }
    
    else if ( a >= n )
    {
        ans = jacobi(a%n, n);
    }
    
    else if ( a % 2 == 0 )
    {
        ans = jacobi(2,n)*jacobi(a/2, n);
    }
    
    else
    {
        ans = ( a % 4 == 3 && n % 4 == 3 ) ? -jacobi(n,a) : jacobi(n,a);
    }

    return ans;

}


long long int compute(fraction input) 
{
    int result;
    do {
        // Once the numerator become 1, then the whole thing will be 1
        if(input.numerator == 1) 
        {
            input.denominator = 1;
            break;
        }

        // If numerator is even excluding 2, since if the numerator
        // is 2, then it should go to prop 2
        if(isEven(input.numerator) > 0 && input.numerator != 2) 
        {
            input = prop3(input);
            // after prop 3 must be prop 2
            printf("\nProp 3\n");
            printFraction(input);
        }
        if(input.numerator == 2 && !isEven(input.denominator)) 
        {
            input = prop2(input);
            // Since this is last step, so we manually set the 
            // value and break the iteration
            input.numerator = 1;
            input.denominator = 1;
            printf("\nProp 2\n");
            printFraction(input);
            break;
        }
        if(input.numerator < input.denominator) 
        {
            input = prop4(input);
            printf("\nProp 4\n");
            printFraction(input);
        }
        if(input.numerator > input.denominator  && input.denominator!=1) 
        {
            input = prop1(input);
            printf("\nProp 1\n");
            printFraction(input);
        }
    } while(input.numerator > 0 && input.denominator > 0);
    
    result = fractionToInt(input);
    
    return result;
}

fraction prop1(fraction input) 
{
    input.numerator = input.numerator % input.denominator;
    return input;
}

fraction prop2(fraction input) 
{
    long long int n = input.denominator % 8;
    if(n != 1 || n != 3) {
        n += 8;
    }
    printf("%lld\n",n);
    long long int val =  (n == 1 || n == 7) ? 1 : -1;
    val = pow(val, input.exponent);
    if(val == 1) 
    { 
        input.sign *= POSITIVE; 
    }
    else 
    { 
        input.sign *= NEGATIVE; 
    }
    return input;
}

fraction prop3(fraction input) 
{
    fraction temp = input;
    temp.numerator = 2;
    int exponent = 0;
    while(isEven(input.numerator) > 0) 
    {
        input.numerator /= 2;
        exponent++;
    }
    temp.exponent = exponent;
    temp = prop2(temp); // Just want to know the SIGN
    if(temp.sign == -1) 
    { // if sign is negative
        // i.e. -1 x -1 = 1
        input.sign *= temp.sign;
    }
    printf("\nProp 3:\n");
    
    printFraction(temp);
    
    printf("^%d x ", temp.exponent);
    printFraction(input);
    
    if(input.numerator==1)
        input.denominator=1;
    return input;
}

fraction prop4(fraction input) 
{
    // swapping
    long long int denominator = input.denominator;
    input.denominator = input.numerator;
    input.numerator = denominator;

    int mod_denominator = input.denominator % 4;
    int mod_numerator = input.numerator % 4;

    if(mod_numerator == 3 && mod_denominator == 3) 
    {
        input.sign *= NEGATIVE;
    }
    return input;
}

long long int fractionToInt(fraction input) 
{
    long long int result = (int)(input.numerator / input.denominator);
    return (input.sign * result);
}

long long int isEven(long long int num) 
{
    return (num % 2 == 0) ? 1 : 0;
}

void printFraction(fraction frac) 
{
    if(frac.sign == -1) 
        printf("-");
    printf("(%lld/%lld)", frac.numerator, frac.denominator);
}
