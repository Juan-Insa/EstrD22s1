#include <iostream>
using namespace std;

struct PokeSt;

typedef PokeSt* Pokemon;
typedef string TipoDePokemon;

Pokemon consPokemon(TipoDePokemon tipo);
TipoDePokemon tipoDePokemon(Pokemon p);
int energia(Pokemon p);
void perderEnergia(int energia, Pokemon p);
bool superaA(Pokemon p1, Pokemon p2);
void showPokemon(Pokemon p);




