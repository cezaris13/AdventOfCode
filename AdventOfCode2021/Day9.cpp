#include <bits/stdc++.h>

using namespace std;
int sumRecursive (vector<vector<bool>> &visited,vector<string> mapp, int x, int y){
    if (mapp[x][y]-48>8)
        return 0;
    if(visited[x][y])
        return 0;
    int sum=0;
    visited[x][y]=1;
    if(x==0){
        sum+= sumRecursive(visited,mapp, x+1, y);
    }
    else if (x==mapp.size()-1){
        sum+= sumRecursive(visited,mapp, x-1, y);
    }
    else{
        sum+= sumRecursive(visited,mapp, x-1, y) + sumRecursive(visited,mapp, x+1, y);
    }
    if(y==0){
        sum+= sumRecursive(visited,mapp, x, y+1);
    }
    else if(y==mapp[0].size()-1){
        sum+= sumRecursive(visited,mapp, x, y-1);
    }
    else{
        sum+= sumRecursive(visited,mapp, x, y-1) + sumRecursive(visited,mapp, x, y+1);
    }
    return 1+sum;
}

int main()
{// part 1
  freopen("input.txt","r",stdin);
  string currValue;
  vector<string> elements;
  while(!cin.eof()){
    cin>>currValue;
    elements.push_back(currValue);
  }
  vector<vector<bool>> visited;
  for(int i=0;i<elements.size();i++){
      vector<bool> temp;
      for(int j=0;j<elements[i].size();j++){
          temp.push_back(0);
      }
      visited.push_back(temp);
  }

  int lowPoints=0;
  vector<int> sizes;
  vector<pair<int,int>> cords;
  for(int i=0;i<=elements.size();i++){
      for(int j=0;j<elements[i].size();j++){
          int minAdj=100;
          if(i==0){
              minAdj = min(minAdj,(int)elements[i+1][j]);
          }
          else if (i==elements.size()-1){
              minAdj = min(minAdj,(int)elements[i-1][j]);
          }
          else{
              minAdj = min(min(minAdj,(int)elements[i-1][j]),(int)elements[i+1][j]);
          }
          if(j==0){
              minAdj = min(minAdj,(int)elements[i][j+1]);
          }
          else if(j==elements[i].size()-1){
              minAdj = min(minAdj,(int)elements[i][j-1]);
          }
          else{
              minAdj = min(min(minAdj,(int)elements[i][j-1]),(int)elements[i][j+1]);
          }
          if(minAdj > (int)elements[i][j]){
              cords.push_back(make_pair(i, j));
              // lowPoints=lowPoints+elements[i][j]-48+1;
          }
      }
  }
  elements.pop_back();
  for(auto a : cords){
      sizes.push_back(sumRecursive(visited,elements,a.first,a.second));
  }
  sort(sizes.rbegin(),sizes.rend());
  cout<<sizes[0]*sizes[1]*sizes[2]<<endl;
  // for(auto a : sizes){
  //     cout<<a<<endl;
  // }

  // cout<<lowPoints<<endl;
}
