#include <bits/stdc++.h>

using namespace std;

pair<pair<int,int>,pair<int,int>> GetCoordinates(){
    int x1,y1,x2,y2;
    char comma,space1,dash,more,space2,comma2;
    cin>>x1>>comma>>y1>>dash>>more>>x2>>comma2>>y2;
    pair<pair<int,int>,pair<int,int>> coordinates;
    coordinates.first.first=x1;
    coordinates.first.second=y1;
    coordinates.second.first=x2;
    coordinates.second.second=y2;
    return coordinates;
}
int countMax(vector<pair<pair<int,int>,pair<int,int>>> input){
    int maxValue=-1;
    for(auto element : input){
        maxValue=max(max(max(max(maxValue,element.first.first),element.second.first),element.first.second),element.second.second);
    }
    return maxValue;
}
int main()
{
  freopen("input.txt","r",stdin);
  vector<pair<pair<int,int>,pair<int,int>>> elements;
  while(!cin.eof()){
      pair<pair<int,int>,pair<int,int>> corrds=GetCoordinates();
      elements.push_back(corrds);
  }
  elements.pop_back();
  int maxValue = countMax(elements);
  int mapOfValues[maxValue+1][maxValue+1];
  for(int i=0;i<=maxValue;i++){
      for(int j=0;j<=maxValue;j++){
          mapOfValues[i][j]=0;
      }
  }
  for(auto element : elements){
      if(element.first.second == element.second.second){
          int mini=min(element.first.first,element.second.first);
          int maxi=max(element.first.first,element.second.first);
          for(int i=mini;i<=maxi;i++){
              mapOfValues[i][element.first.second]++;
          }
      }
      else if(element.first.first == element.second.first){
          int mini=min(element.first.second,element.second.second);
          int maxi=max(element.first.second,element.second.second);
          for(int i=mini;i<=maxi;i++){
              mapOfValues[element.first.first][i]++;
          }
      }
      // part 2 diagonals
      else{
          int miniy=min(element.first.second,element.second.second);
          int maxiy=max(element.first.second,element.second.second);
          int k=(element.first.second-element.second.second)/(element.first.first-element.second.first);
          pair<int,int> leftest = min(element.first,element.second);
          if(k==1){
              for(int i=0;i<=maxiy-miniy;i++){
                  mapOfValues[leftest.first+i][leftest.second+i]++;
              }

          }
          else if(k==-1){
              for(int i=0;i<=maxiy-miniy;i++){
                  mapOfValues[leftest.first+i][leftest.second-i]++;
              }
          }
      }
  }
  int count=0;
  for(int i=0;i<=maxValue;i++){
      for(int j=0;j<=maxValue;j++){
          if(mapOfValues[i][j]>1){
              count++;
          }
      }
  }
 //for(int  i=0;i<=maxValue;i++){
 //     for(int j=0;j<=maxValue;j++){
 //         if(mapOfValues[j][i]==0){
 //             cout<<". ";
 //         }
 //         else{
 //             cout<<mapOfValues[j][i]<<" ";
 //         }
 //     }
 //     cout<<endl;
 // }

cout<<count;
}
