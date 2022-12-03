#include <bits/stdc++.h>

using namespace std;

// int main(){
//   int horiz=0,vert=0,number;
//   freopen("input.txt","r",stdin);
//   while(!cin.eof()){
//     string command;
//     cin>>command>>number;
//     if(command =="forward"){
//       horiz+=number;
//     }
//     if(command =="up"){
//       vert-=number;
//     }
//     if(command =="down"){
//       vert+=number;
//     }
//   }
//   cout<<horiz*vert<<endl;
// }
int main(){
  int horiz=0,vert=0,number,aim=0;
  freopen("input.txt","r",stdin);
  while(!cin.eof()){
    string command;
    cout<<"before: "<<horiz<<" "<<vert<<" "<<aim<<endl;
    cin>>command>>number;
    cout<<command<<number<<endl;
    if(command =="forward"){
      horiz+=number;
      vert+=aim*number;
    }
    if(command =="up"){
      aim-=number;
    }
    if(command =="down"){
      aim+=number;
    }
    cout<<"after: "<<horiz<<" "<<vert<<" "<<aim<<endl;
  }
  cout<<vert*horiz<<endl;
}
