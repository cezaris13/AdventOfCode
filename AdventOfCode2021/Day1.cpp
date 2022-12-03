#include <bits/stdc++.h>

using namespace std;

int main()
{
  freopen("input.txt","r",stdin);
  int currValue;
  vector<int> elements;
  while(!cin.eof()){
    cin>>currValue;
    elements.push_back(currValue);
  }
  int currSliding,prevSliding=-1,intCount=0;
  for(int i=0;i<elements.size()-3;i++){
    currSliding=elements[i]+elements[i+1]+elements[i+2];
    if(prevSliding!=-1){
      intCount+=(currSliding-prevSliding>0)?1:0;
    }
    prevSliding=currSliding;
  }
  cout<<intCount<<endl;
}
