#include <iostream>
#include "ArrayList.h"
using namespace std;

// soy user de ArrayList

// Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs){
    int resultado = 0;
    for(int i=1; i <= lengthAL(xs); i++){
        resultado += get(i, xs);
    }
    return resultado;
}

// Incrementa en uno todos los elementos.
void sucesores(ArrayList xs){
    for(int i=1; i <= lengthAL(xs); i++){
        set(i, get(i,xs) + 1, xs);
    }
}

// Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs){
    bool resultado = false;
    for(int i=1; i <= lengthAL(xs); i++){
        resultado = resultado || get(i,xs) == x;
    }
    return resultado;
}

int unoSi(bool cond){
    if (cond){
        return 1;
    }
    return 0;
}

// Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs){
    int resultado = 0;
    for(int i=1; i <= lengthAL(xs); i++){
        resultado += unoSi(get(i, xs) == x);
    }
    return resultado;
}

// dados dos arrayList devuelve el primero con todos los elementos del segundo agregados
// a partir de la posicion dada.
// precondición: la posicion es <= a la capacidad del primer array.
ArrayList agregarElementos(ArrayList xs, ArrayList ys){
    for (int i=1; i <= lengthAL(ys); i++){
        add(get(i, ys), xs);
    }
    return xs;
}

// Crea una nueva lista a partir de la primera y la segunda (en ese orden)
// nota: en este caso no me preocupo por rezise, pero la capacidad queda justa.
ArrayList append(ArrayList xs, ArrayList ys){
    ArrayList zs = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    agregarElementos(zs, xs);
    agregarElementos(zs, ys);
    return zs;
}

// nota: en este caso pude q haya rezise, pero no me preocupo por la capacidad.
ArrayList append2(ArrayList xs, ArrayList ys){
    ArrayList zs = newArrayList();
    agregarElementos(zs, xs);
    agregarElementos(zs, ys);
    return zs;
}

// Devuelve el elemento más chico de la lista.
// nota: debería tener precond: la lista no es vacia.
int minimo(ArrayList xs){
    int minimoVisto = get(1, xs);
    for (int i=2; i <= lengthAL(xs); i++){
        minimoVisto = min(minimoVisto, get(i, xs));
    }
    return minimoVisto;  
}

int main(){
    ArrayList xs = newArrayList();
    add(2,xs); add(1,xs); add(4,xs); add(6,xs);

    ArrayList ys = newArrayList();
    add(5,ys); add(12,ys); add(5,ys); add(7,ys);

    showArrayList(xs);
    
    cout << sumatoria(xs) << endl;
    sucesores(xs);
    showArrayList(xs);
    cout << pertenece(7, ys) << endl;
    cout << apariciones(5, ys) << endl;
    showArrayList(append(xs, ys));
    cout << minimo(ys) << endl;
    /*
    cout << sumatoria(xs) << endl;
    sucesores(xs);
    cout << pertenece(6, xs) << endl;
    cout << apariciones(5, ys) << endl;
    showArrayList(append(xs, ys));
    cout << minimo(xs) << endl;
    */

    
}