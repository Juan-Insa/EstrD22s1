#include <iostream>
using namespace std;

// Propósito: imprime n veces un string s utilizando iteración.
void printN(int n, string s){
    while(n>0){
        cout << s << endl;
        n--;
    }
}

// Propósito: imprime n veces un string s utilizando recursión.
void printNRec(int n, string s){
    if (n>0){
        cout << s << endl;
        printNRec(n-1,s);
    }
}

// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
// utiliza iteración.
void cuentaRegresiva(int n){
    while(n>=0){
        cout << n << endl;
        n--;
    }       
}

// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
// precondición: n es positivo.
// utiliza recursión.
void cuentaRegresivaRec(int n){
    if (n>=0){
        cout << n << endl;
        cuentaRegresivaRec(n-1);
    }     
}

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
// utiliza iteración.
void desdeCeroHastaN(int n){
    for(int i=0; i<=n; i++){
        cout << i << endl;
    }
}

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
// utiliza recursion.
void desdeCeroHastaNRec(int n){
    if (n >= 0){
        desdeCeroHastaNRec(n-1);
        cout << n << endl;
    }
}

// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
// utiliza iteración.
int mult(int n, int m){
    int resultado = 0; 
    while(n > 0){
        resultado += m;
        n--;
    }
    return resultado; 
}

// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
// utiliza recursión.
int multRec(int n, int m){
    if (n>0){
        return m + multRec(n-1,m);
    } else {
        return 0;
    }    
}

// iteración
void primerosN(int n, string s){
    for(int i=0; i < n; i++){
        cout << s[i] << endl;        
    }
}

// recursión
void primerosNRec(int n, string s){
    if (n <= 0){
        primerosNRec(n-1, s);
        cout << s[n] << endl;   
    }
}

// iteración
bool pertenece(char c, string s){
    int i = 0;
    while (s[i] != c && s[i] != NULL ){
        i++;
    }
    return c == s[i];
}

// con size me ahorro la subtarea
bool perteneceRec(char c, string s){
    if (s.size() > 0){
        return s[0] == c || perteneceRec(c, s.erase(0,1));
    }
}

// no compila con NULL 
bool belongsRec(char c, string s, int n){
    if(s[n] == NULL){
        return s[n] == c;
    } else {
        return s[n] == c || belongsRec(c, s, n++);
    }     
}

// recursión
bool perteneceRec2(char c, string s){
    return belongsRec(c, s, 0);
}

int unoSi(bool cond){
       if (cond){
           return 1;
       } else {
           return 0;
       }
}

// iterativo
int apariciones(char c, string s){
    int apariciones = 0;
    for (int i=0; i < s.size(); i++){
        apariciones += unoSi(s[i] == c);
    }
    return apariciones;
}

// con size
int aparicionesRec(char c, string s){
    if(s.size() == 0){
        return 0;
    } else {
        return unoSi(c == s[0]) + aparicionesRec(c, s.erase(0,1));
    }
}



int main(){
    cout << apariciones('a', "calamar") << endl;
}