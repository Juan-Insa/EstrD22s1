#include <iostream>
#include "Pokemon.h"
using namespace std;

struct PokeSt{
    TipoDePokemon tipo;
    int vida;
};

// Dado un tipo devuelve un pokémon con 100 % de energía.
Pokemon consPokemon(TipoDePokemon tipo){
    Pokemon p = new PokeSt;
    p->vida = 100;
    p->tipo = tipo;
    return p;
}

// Devuelve el tipo de un pokémon.
TipoDePokemon tipoDePokemon(Pokemon p){
    return p->tipo;
}

// Devuelve el porcentaje de energía.
int energia(Pokemon p){
    return p->vida;
}

// Le resta energía al pokémon.
void perderEnergia(int energia, Pokemon p){
    p->vida -= energia;
}

// indica si el primer tipo de pokémon es superior al segundo.
// nota: Agua supera a fuego, fuego a planta y planta a agua. Cualquier otro caso es falso.
bool esMayorA(TipoDePokemon t1, TipoDePokemon t2){
    return (t1 == "Agua"   && t2 == "Fuego")  ||
           (t1 == "Fuego"  && t2 == "Planta") ||
           (t1 == "Planta" && t2 == "Agua");
}

// Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
// a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool superaA(Pokemon p1, Pokemon p2){
    return esMayorA(p1->tipo, p2->tipo);
}

void showPokemon(Pokemon p){
    cout << "Tipo: " << p->tipo << endl;
    cout << "Vida: " << p->vida << endl;
}

/*
int main(){
    Pokemon p1 = consPokemon("Agua");
    Pokemon p2 = consPokemon("Fuego");
    perderEnergia(30, p1);
    showPokemon(p1);
    showPokemon(p2);
    cout << superaA(p2,p1) << endl;
}
*/




