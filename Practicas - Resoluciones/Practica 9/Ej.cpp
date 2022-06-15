#include <iostream>
using namespace std;

// Precondicion: c1 < c2
   void printFromTo(char c1, char c2) {
       for(int i = 0; c1 + i <= c2; i++) {
            cout << c1 + i << ", ";
       }
       cout << endl;
   }



// Precondicion: n >= 0
   int fc(int n) {
       int x = 1;
       while(n > 0) {
           x = x * n;
           n--;
       }
       return x;
   }


// Precondicion: n <= m
   int ft(int n, int m) {
       if (n == m) {
       return n;
       }
       return n + ft(n+1, m);
   }

// Precondicion: n <= m
   int ft2(int n, int m) {
       int total = 0;
       while(n<=m){
           total += n;
           n++;
       }
       return total;
   }

    int main() {
        cout << ft2(6,10) << endl; 
    }
