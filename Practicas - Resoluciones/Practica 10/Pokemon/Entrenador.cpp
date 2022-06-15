#include <iostream>
#include "Entrenador.h"
#include "Pokemon.h"
using namespace std;

// en este caso soy implementador de Entrenador y usuario de Pokemon (no te olvides).

struct EntrenadorSt{
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

// Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
// un entrenador.
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    Entrenador e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemon = pokemon;
    return e;
}

// Devuelve el nombre del entrenador.
string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

// Devuelve la cantidad de pokémon que posee el entrenador.
int cantidadDePokemon(Entrenador e){
    return e->cantPokemon;
}

// describe si el pokemon es del tipo dado.
bool esDeTipo(Pokemon p, TipoDePokemon t){
    return tipoDePokemon(p) == t;
}

int unoSi(bool cond){
    if (cond){
        return 1;
    }
    return 0;
}

// Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int total = 0;
    for(int i=0; i < e->cantPokemon; i++){
        total += unoSi(esDeTipo(e->pokemon[i], tipo));
    }
    return total;
}

// Devuelve el pokémon número i de los pokémon del entrenador.
// Precondición: existen al menos i − 1 pokémon.
Pokemon pokemonNro(int i, Entrenador e){
    return e->pokemon[i-1];
}

// dado un pokemon y un entrenador, indica si, para cada pokémon del entrenador, el pokémon
// dado los supera.
bool leGanaATodosLosDe(Pokemon p, Entrenador e){
    bool resultado = true;
    for(int i=0; i < e->cantPokemon; i++){
        resultado = resultado && superaA(p, e->pokemon[i]);
    }
    return resultado;
}

// Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
// posee al menos un pokémon que le gane.
bool leGanaATodos(Entrenador e1, Entrenador e2){
    bool resultado = false;
    for(int i=0; i < e1->cantPokemon; i++){
        resultado = resultado || leGanaATodosLosDe(e1->pokemon[i], e2);
    }
    return resultado;
}

void imprimirPokemon(Entrenador e){
    cout << "Pokemon <- [ ";
    for (int i=0; i < e->cantPokemon; i++){
        if(i>0) {cout << ", "; }
        cout << tipoDePokemon((e->pokemon[i]));
    }
    cout << " ]" << endl;
}

// crea un array de 2 Pokémon.
Pokemon* arrayDePokes(Pokemon p1, Pokemon p2){
    Pokemon* pokes = new Pokemon[2];
    pokes[0] = p1;
    pokes[1] = p2;
    return pokes;
}

// pokémon
Pokemon p1 = consPokemon("Agua");
Pokemon p2 = consPokemon("Planta");
Pokemon p3 = consPokemon("Fuego");
Pokemon p4 = consPokemon("Fuego");

// arrays de pokémon
Pokemon* pokes1 = arrayDePokes(p1, p2);
Pokemon* pokes2 = arrayDePokes(p3, p4);

void showEntrenador(Entrenador e){
    cout << "Nombre: " << e->nombre << endl;
    cout << "Cantidad de Pokemon: " << e->cantPokemon << endl;
    imprimirPokemon(e);
}

int main(){
    Entrenador e1 = consEntrenador("lalo", 2, pokes1);
    Entrenador e2 = consEntrenador("howard", 2, pokes2);
    showEntrenador(e1);
    cout << "" << endl;
    showEntrenador(e2);
    cout << cantidadDePokemonDe("Fuego", e2) << endl;
    showPokemon(pokemonNro(2, e1));
    cout << leGanaATodos(e1,e2) << endl; 
    cout << leGanaATodos(e2,e1) << endl; 
}








