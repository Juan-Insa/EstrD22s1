#include <iostream>
#include "Persona.h"
using namespace std;

struct PersonaSt{
    string nombre;
    int    edad;
};

// Devuelve el nombre de una persona
Persona consPersona(string nombre, int edad){
    Persona p = new PersonaSt;
    p->nombre = nombre; p->edad = edad;
    return p;
}

// Devuelve el nombre de una persona
string nombre(Persona p){
    return p->nombre;
}

// Devuelve la edad de una persona
int edad(Persona p){
    return p->edad;
}

// Aumenta en uno la edad de la persona.
void crecer(Persona p){
    p->edad++;
}


// Modifica el nombre una persona.
void cambioDeNombre(string nombre, Persona p2){
    p2->nombre = nombre;
}

// Dadas dos personas indica si la primera es mayor que la segunda
bool esMayorQueLaOtra(Persona p1, Persona p2){
    return p1->edad > p2->edad;
}

// Dadas dos personas devuelve a la persona que sea mayor.
Persona laQueEsMayor(Persona p1, Persona p2){
    if (esMayorQueLaOtra(p1, p2)){
        return p1;
    } else {
        return p2;
    }
}

void showPersona(Persona p){
    cout << "Nombre: " << p->nombre << endl;
    cout << "Edad: " << p->edad << endl;
}

int main(){
    Persona p1 = consPersona("carlos", 40);
    Persona p2 = consPersona("juancho", 26);
    crecer(p1);
    cambioDeNombre("pepe", p1);
    showPersona(p1);
    cout << esMayorQueLaOtra(p2, p1) << endl;
    showPersona(laQueEsMayor(p1, p2));
}






