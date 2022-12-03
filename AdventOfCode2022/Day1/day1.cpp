#include <bits/stdc++.h>

using namespace std;

int toInt(string input);

// int main() {// part 1
//    freopen("input","r",stdin);
//    int maxElfCallories = -1;
//    int tempElfCallories = 0;
//    string input="";
//    while(!cin.eof()){
//        getline(cin,input);
//        if(input.size() == 0) {
//            maxElfCallories = tempElfCallories > maxElfCallories ? tempElfCallories : maxElfCallories;
//            tempElfCallories = 0;
//        }
//        else {
//            tempElfCallories += toInt(input);
//        }
//    }
//    printf("max elf callories: %d\n", maxElfCallories);
// }

int main() {
   freopen("input","r",stdin);
   int maxElfCallories = -1;
   int tempElfCallories = 0;
   string input="";
   set<int> elfCallories;
   while(!cin.eof()){
       getline(cin,input);
       if(input.size() == 0) {
           maxElfCallories = tempElfCallories > maxElfCallories ? tempElfCallories : maxElfCallories;
           elfCallories.insert(tempElfCallories);
           tempElfCallories = 0;
       }
       else {
           tempElfCallories += toInt(input);
       }
   }
   int sumOfThree = 0;
   int i=0;
   for(set<int>::reverse_iterator it=elfCallories.rbegin(); it!=elfCallories.rend(); ++it){
       if(i < 3) {
           sumOfThree += (*it);
           printf("%d elf callories: %d\n",i,(*it));
           i++;
       }
   }

   printf("sum of 3 elf callories: %d\n", sumOfThree);
}


int toInt(string input){
    return stoi(input);
}
