#include <bits/stdc++.h>

using namespace std;
#define MAPMAX 10

void readData(vector<vector<int>> &input){
    while(!cin.eof()){
        long currValue;
        vector<int> temp;
        cin>>currValue;
        if(currValue==0)
            break;
        while(currValue>0){
            temp.push_back(currValue%10);
            currValue/=10;
        }
        reverse(temp.begin(), temp.end());
        input.push_back(temp);
    }
}
void printMap(vector<vector<int>> elements){
    for(int i=0;i<elements.size();i++){
      for(int j=0;j<elements[i].size();j++){
          cout<<elements[i][j]<<" ";
      }
              cout<<endl;
  }
    cout<<endl;
}
int part1(vector<vector<int>> input){
    for(int i=1;i<input.size();i++){
        input[i][0]+=input[i-1][0];
    }
    for(int i=1;i<input[0].size();i++){
        input[0][i]+=input[0][i-1];
    }
    for(int i=1;i<input.size();i++){
        for(int j=1;j<input[i].size();j++){
            input[i][j]+=min(input[i-1][j],input[i][j-1]);
        }
    }
    return input[input.size()-1][input[0].size()-1]-input[0][0];
}

int main(){
    freopen("input.txt", "r", stdin);
    vector<vector<int>> input;
    readData(input);
    printMap(input);
    cout<<part1(input);

}
