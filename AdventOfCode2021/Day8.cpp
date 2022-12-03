#include <bits/stdc++.h>

using namespace std;

void readValues(vector<string> &patterns,vector<string> &output){
    for(int i=0;i<10;i++){
        string input;
        cin>>input;
        patterns.push_back(input);
    }
    string delim;
    cin>>delim;
    for(int i=0;i<4;i++){
        string input;
        cin>>input;
        output.push_back(input);
    }
}
// part 1
// int main(){
//   freopen("input.txt","r",stdin);
//   int count = 0;
//   while(!cin.eof()){
//       vector<string> inputValues,outputValues;
//       readValues(inputValues, outputValues);
//       for(int i=0;i<outputValues.size();i++){
//           int ssize = outputValues[i].size();
//           if(ssize == 2 || ssize == 4 || ssize == 3 || ssize ==7){
//               count++;
//           }
//       }
//   }
//   cout<<count;
// }
bool firstContainsSecond(string first, string second){
    for(int i=0;i<second.size();i++){
        size_t found = first.find(second[i]);
        if(found == string::npos){
            return false;
        }
    }
    return true;
}

bool comp(string first ,string second){
    return first.size()<second.size();
}
// part 2
int main(){
  freopen("input.txt","r",stdin);
  int count = 0;
  while(!cin.eof()){
      vector<string> inputValues,outputValues;
      readValues(inputValues, outputValues);
      if(inputValues.size()==0 || inputValues[0].size()==0)
          break;
      string digits[10];
      sort(inputValues.rbegin(),inputValues.rend(),comp);
      for(int i=0;i<inputValues.size();i++){
          int ssize = inputValues[i].size();
          sort(inputValues[i].begin(),inputValues[i].end());
          if(ssize == 2){
              digits[1]=inputValues[i];
          }
          else if(ssize == 4){
              digits[4]=inputValues[i];
          }
          else if(ssize == 3){
              digits[7]=inputValues[i];
          }
          else if(ssize ==7){
              digits[8]=inputValues[i];
          }
      }
      string sixMissing ="";
      for(int i=0;i<inputValues.size();i++){
          int ssize = inputValues[i].size();
          if(ssize == 6){
              //contains 4 => 9
              if(firstContainsSecond(inputValues[i],digits[4])){
                  digits[9]=inputValues[i];
              }
              //contains 7 => 0
              else if(firstContainsSecond(inputValues[i],digits[7])){
                  digits[0]=inputValues[i];
              }
              else {
                  digits[6]=inputValues[i];
                  string eight = digits[8];
                  for(int j=0;j<digits[6].size();j++){
                      eight.erase(remove(eight.begin(),eight.end(),digits[6][j]),eight.end());
                  }
                  sixMissing = eight;
              }
          }
          else if(ssize == 5){
              size_t found = inputValues[i].find(sixMissing);
              if(firstContainsSecond(inputValues[i],digits[1])){
                  digits[3]=inputValues[i];
              }
              else if(found != string::npos){
                  //if digit contains missing stick from 6 -> 2
                  digits[2]=inputValues[i];
              }
              else{
                  digits[5]=inputValues[i];
              }
              // contains 1 => 3
          }
      }
      for(int i=0;i<10;i++){
          cout<<i<< " "<<digits[i]<<endl;

      }

      //what if not all numbers are found
      int number=0;
      for(int i=0;i<outputValues.size();i++){
          int val = -1;
          number*=10;
          sort(outputValues[i].begin(),outputValues[i].end());
          for(int j=0;j<10;j++){
              if(digits[j]==outputValues[i]){
                  val=j;
              }
          }
          cout<<outputValues[i]<<" "<<val<<endl;
          number+=val;
      }
      count+=number;
      cout<<endl;
  }
      cout<<count<<endl;
}
