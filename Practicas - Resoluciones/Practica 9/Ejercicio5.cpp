#include <iostream>
using namespace std;

// Ejercicio 5

struct Fraccion{
    int numerador;
    int denominador;
};

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return f.numerador / f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
Fraccion multF(Fraccion f1, Fraccion f2){
    return consFraccion(f1.numerador * f2.numerador, f1.denominador * f2.denominador);
}

bool esDivisorDe(int n, int m){
    return (n % m) == 0;
}

int mcm(int n, int m){
    int i = min(n, m);
    while(!esDivisorDe(i,n) && !esDivisorDe(i,m)){
        i--;
    }
    return i;
}

// Propósito: devuelve una fracción que resulta de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p){
    int divisor = mcm(p.numerador, p.denominador);
    return consFraccion(p.numerador / divisor, p.denominador / divisor);
}

// Propósito: devuelve una fracción que resulta de sumar las dos fracciones dadas.
Fraccion sumF(Fraccion f1, Fraccion f2){
    if (f1.denominador == f2.denominador){
        int numerador = f1.numerador + f2.numerador;
        return consFraccion(numerador, f1.denominador);
    } else {
        int numerador = (f1.numerador * f2.denominador) + (f2.numerador * f1.denominador);
        int denominador = f1.denominador * f2.denominador;
        return consFraccion(numerador, denominador);
    }
}

void imprimirFraccion(Fraccion f){
    cout << "numerador   -> " << f.numerador << endl;
    cout << "denominador -> " << f.denominador << endl;
}

int main(){
    Fraccion f1 = consFraccion(10,5);
    Fraccion f2 = consFraccion(5,4);
    imprimirFraccion(sumF(f1, f2));
}