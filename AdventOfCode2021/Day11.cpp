#include <bits/stdc++.h>
#define MAPMAX 10
using namespace std;

void readData(vector<vector<int>> &input){
    for(int i=0;i<MAPMAX;i++){
        long currValue;
        vector<int> temp;
        cin>>currValue;
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
int updateOctopus(vector<vector<int>> &mapp){
    int zeroCount=0;
    vector<vector<bool>> visited;

    for(int i=0;i<10;i++){
        vector<bool> temp;
        for(int j=0;j<10;j++){
            temp.push_back(false);
        }
        visited.push_back(temp);
    }
    queue<pair<int,int>> next;
    for(int i=0;i<MAPMAX;i++){
        for(int j=0;j<MAPMAX;j++){
            mapp[i][j]++;
            if(mapp[i][j]>9){
                mapp[i][j]=0;
                zeroCount++;
                visited[i][j]=true;
                next.push(make_pair(i+1, j));
                next.push(make_pair(i-1, j));
                next.push(make_pair(i+1, j+1));
                next.push(make_pair(i-1, j+1));
                next.push(make_pair(i+1, j-1));
                next.push(make_pair(i-1, j-1));
                next.push(make_pair(i, j+1));
                next.push(make_pair(i, j-1));
            }
        }
    }
    while(!next.empty()){
        pair<int,int> currValue=next.front();
        next.pop();
        if(currValue.first<0 || currValue.first>9 || currValue.second <0 || currValue.second>9){
            continue;
        }
       if(visited[currValue.first][currValue.second]){
            continue;
       }
        mapp[currValue.first][currValue.second]++;
        if(mapp[currValue.first][currValue.second]>9){
            mapp[currValue.first][currValue.second]=0;
            zeroCount++;
            visited[currValue.first][currValue.second]=true;
            next.push(make_pair(currValue.first+1, currValue.second));
            next.push(make_pair(currValue.first-1, currValue.second));
            next.push(make_pair(currValue.first+1, currValue.second+1));
            next.push(make_pair(currValue.first-1, currValue.second+1));
            next.push(make_pair(currValue.first+1, currValue.second-1));
            next.push(make_pair(currValue.first-1, currValue.second-1));
            next.push(make_pair(currValue.first, currValue.second+1));
            next.push(make_pair(currValue.first, currValue.second-1));
        }
    }
    return zeroCount;
}
int main()
{
  freopen("input.txt","r",stdin);
  vector<vector<int>> elements;
  readData(elements);
  // printMap(elements);
  long totalCount=0;
  for(int i=0;;i++){
          int returnedCount=updateOctopus(elements);
          totalCount+=returnedCount;
          if(returnedCount==100){// part 2 count when all octopuses flash at the same time
              cout<<i<<endl;
              break;
          }
      // printMap(elements);
  }
  cout<<totalCount<<endl;
}
