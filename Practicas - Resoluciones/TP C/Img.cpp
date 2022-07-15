#include "Img.h"

#define DIR        int
#define HOJA       99
#define HORIZONTAL 42
#define VERTICAL   17
#define unitSize   50

struct ITreeSt {     
    DIR      division;    
    Color    color;       
    ITreeSt* first;    
    ITreeSt* second;
};
 /* INV.REP.
    * el valor de DIR admitidos son HOJA(99), HORIZONTAL(42) O VERTICAL(17).
    * si el valor DIR es HOJA, Color no puede ser NULL.
    * si el valor DIR es HOJA, first y second son NULL.
    * si el valor DIR es HORIZONTAL o VERTICAL, Color es NULL.
    OBS: si division es
      - HOJA, entonces color es el color del bloque representado  
      - HORIZONTAL, entonces first es la parte izquierda y second la derecha
      - VERTICAL, entonces first es la parte superior y second la inferior
 */

struct ImgSt {
    int height;
    int width;
    int size;
    ITreeSt* t;
};
 /* INV.REP.
    * el valor de size es menor o igual a width * height.
    * el valor de size es igual a la cantidad de hojas de t.
    * todo nodo de t con campo DIR VERTICAL tiene hijos con DIR HORIZONTAL u HOJA.
    * todo nodo de t con campo DIR HORIZONTAL tiene hijos con DIR VERTICAL u HOJA.
    * width y height son valores potencia de 2.
    * width y height tienen el mismo valor. Obs: forman un cuadrado.
 */

//---------------------------------------------------------
// sizeImg
//---------------------------------------------------------

// devuelve la cantidad de bloques de la imágen.
// Precondición: la imagen no es NULL.
// obs: serían la cantidad de hojas que componen el árbol de la img.
int sizeImg(Img img) {
  return img->size;
}

//---------------------------------------------------------
// createImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
// PRECOND: iw, ih, fw, fh y n son potencia de 2.
ITreeSt* loadIT(int iw, int ih
               ,int fw, int fh
               ,int n, Matrix m, DIR d) {
  ITreeSt* t = new ITreeSt;
  t->division = d;
  if (n == 1) {
    t->division = HOJA;
    t->color  = M_getAt(m, iw, ih);
    t->first  = NULL;
    t->second = NULL;
  }
  else if (t->division == HORIZONTAL){
    t->color  = NULL;
    t->first  = loadIT(iw, ih,        fw, fh/2, n/2, m, VERTICAL);
    t->second = loadIT(iw, ih+(fh/2), fw, fh/2, n/2, m, VERTICAL);
  }
  else if (t->division == VERTICAL){
    t->color  = NULL;
    t->first  = loadIT(iw,        ih, fw/2, fh, n/2, m, HORIZONTAL);
    t->second = loadIT(iw+(fw/2), ih, fw/2, fh, n/2, m, HORIZONTAL);
  }
  return t;
}

// genera una imagen a partir de la matriz y dimension dada.
// PRECOND: w es potencia de 2, m es de w*w
Img createImg(Matrix m, int w) {
  Img i = new ImgSt;
  i->height = w;
  i->width  = w;
  i->size   = w*w;
  i->t      = loadIT(1, 1, w, w, w*w, m, HORIZONTAL);
  M_delete(m);
  return i;
}

//---------------------------------------------------------
// CompressImg
//---------------------------------------------------------
// dado un árbol, lo comprime definiéndolo como una hoja del color de sus nodos hijos si estos tienen
// un mismo color entre si y devueve el valor 1. De lo contrario no hace nada y devuelve 0. 
// Precondición: el árbol dado no es NULL y su DIR no es HOJA.
// OBS: el valor devuelto corresponde a si se comprime el árbol o no. Esto sirve para recalcular
// el size.
int buildT(ITreeSt* t) {
  // si los hijos son hojas con mismo color
  if (t->first->division  == HOJA && 
      t->second->division == HOJA && 
      t->first->color == t->second->color) {

    // modifico al padre como hoja del color de los hijos
    t->division = HOJA;
    t->color  = t->first->color;

    // elimino los hijos
    delete t->first;
    delete t->second;

    // los declaro NULL
    t->first  = NULL;
    t->second = NULL;

    return 1;
  }
  return 0;
}

// AUXILIAR SUGERIDA
// Precondición: el árbol dado no es NULL.
// OBS: el int retornado es la cantidad de hojas comprimidas.
int CompressIT(ITreeSt* t) {
  if (t->division != HOJA) {
    return CompressIT(t->first) + CompressIT(t->second) + buildT(t);
  }
  return 0;
}

// Precondición: la imagen no es NULL.
void CompressImg(Img img) {
  img->size -= CompressIT(img->t);
}

//---------------------------------------------------------
// RenderImg
//---------------------------------------------------------
// Precondición: el DIR del árbol dado es HOJA.
void RenderBlock(int x, int y, int w, int h, ITreeSt* t) {
  cout << 
    "\n<rect"
      << " x=\""      << unitSize * x << "\""
      << " y=\""      << unitSize * y << "\""
      << " width=\""  << unitSize * w << "\""
      << " height=\"" << unitSize * h << "\""
      << " style=\"fill:"; RenderColor(t->color,10); 
  cout <<
    ";stroke-width:3;stroke:rgb(0,0,0)" "\"/>";
}

// AUXILIAR SUGERIDA
void RenderIT(int x, int y, int w, int h, ITreeSt* t) {
  if (t->division == HOJA) {
    RenderBlock(x,y,w,h,t);
  }
  else if (t->division == HORIZONTAL) {
    RenderIT(x, y,       w, h/2, t->first);
    RenderIT(x, y+(h/2), w, h/2, t->second);
  }
  else if (t->division == VERTICAL) {
    RenderIT(x,       y, w/2, h, t->first);
    RenderIT(x+(w/2), y, w/2, h, t->second);
  }
}

// Precondición: la imágen no es NULL.
void RenderImg(Img img) {
  int w = img->width;
  int h = img->height;
  ITreeSt* t = img->t;
  cout << "<svg height=\"" << h * unitSize << "\""
         <<    " width=\"" << w * unitSize 
       << "\">";
            RenderIT(0, 0, w, h, t);
  cout << "\n</svg>";
} 
