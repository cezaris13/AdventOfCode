#include <bits/stdc++.h>

using namespace std;

int main()
{
  freopen("input.txt","r",stdin);
  string currValue;
  vector<string> elements;
  while(!cin.eof()){
    cin>>currValue;
    elements.push_back(currValue);
  }
  map<char,int> illegalBrackets;
  vector<long> sumScores;
  for (auto element : elements){
    stack<char> openingBrackets;
    bool skip=false;
    cout<<element<<endl;
      for(auto bracket : element){
          if(bracket == '{' || bracket =='[' || bracket == '(' || bracket == '<'){
              openingBrackets.push(bracket);
          }
          else if(!openingBrackets.empty()){
              if(openingBrackets.top() == '{' && bracket == '}'){
                  openingBrackets.pop();
              }
              else if(openingBrackets.top() == '[' && bracket == ']'){
                  openingBrackets.pop();
              }
              else if(openingBrackets.top() == '(' && bracket == ')'){
                  openingBrackets.pop();
              }
              else if(openingBrackets.top() == '<' && bracket == '>'){
                  openingBrackets.pop();
              }
              else{
                  illegalBrackets[bracket]++;
                  cout<<"expected: "<<openingBrackets.top()<<" got: "<<bracket<<endl;
                  skip=true;// part 2
                  break;
              }
          }
      }
      if(!skip){// part 2 counts correctly
          long score = 0;
          while(!openingBrackets.empty()){
              if(openingBrackets.top() == '{'){
                  score=score*5+3;
              }
              else if(openingBrackets.top() == '[' ){
                  score=score*5+2;
              }
              else if(openingBrackets.top() == '('){
                  score=score*5+1;
              }
              else if(openingBrackets.top() == '<'){
                  score=score*5+4;
              }
              openingBrackets.pop();
          }
          sumScores.push_back(score);
      }
  }
  sort(sumScores.begin(),sumScores.end());
  int cost=0;
  map<char,int> costs;
  costs[')']=3;
  costs[']']=57;
  costs['}']=1197;
  costs['>']=25137;
  for(auto a : illegalBrackets){//387363- part 1
      cost+=costs[a.first]*a.second;// part 2 - 4330777059
      cout<<costs[a.first]<<" "<<a.first<<" "<<a.second<<endl;
  }
  cout<<cost<<endl;
  cout<< sumScores[sumScores.size()/2]<<endl;
}
