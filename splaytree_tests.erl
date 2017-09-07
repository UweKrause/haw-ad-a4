%%%-------------------------------------------------------------------
%%% @author Andreas Hoerster
%%% @copyright (C) 2017, <Team 8>
%%% @doc HAW BAI-3 ADP-P4
%%%
%%% @end
%%% Created : 09. June 2017 12:28
%%%-------------------------------------------------------------------
%%% This module functions to implement a self sorting binary tree.
%%% Whichs is also called a splaytree. Theses are the tests for it..
%%%-------------------------------------------------------------------
-module(splaytree_tests).
-include_lib("eunit/include/eunit.hrl").


isSplayTreeBinTree_test() ->
  {A,B,C,D,E}=validSplayTrees(),
  ?assert(btree:isBT(A)),
  ?assert(btree:isBT(B)),
  ?assert(btree:isBT(C)),
  ?assert(btree:isBT(D)),
  ?assert(btree:isBT(E)).

initBT_test() ->
  {A,B,_,D,_}=validSplayTrees(),
  ?assertEqual(D,splaytree:initBT()),
  ?assertNotEqual(A,splaytree:initBT()),
  ?assertNotEqual(B,splaytree:initBT()),
  ?assertNotEqual({},splaytree:initBT()).

isEmptyBT_test() ->
  {A,B,C,D,E}=validSplayTrees(),
  ?assertNot(splaytree:isEmptyBT(A)),
  ?assertNot(splaytree:isEmptyBT(B)),
  ?assertNot(splaytree:isEmptyBT(C)),
  ?assert(splaytree:isEmptyBT(D)),
  ?assertNot(splaytree:isEmptyBT(E)).

equalBT_test() ->
  {A,B,C,D,E}=validSplayTrees(),
  {A2,B2,C2,D2,E2}=validSplayTrees(),
  ?assert(splaytree:equalBT(A,A2)),
  ?assert(splaytree:equalBT(B,B2)),
  ?assert(splaytree:equalBT(C,C2)),
  ?assert(splaytree:equalBT(D,D2)),
  ?assert(splaytree:equalBT(E,E2)),
  ?assertNot(splaytree:equalBT(A,E2)),
  ?assertNot(splaytree:equalBT(B,D2)),
  ?assertNot(splaytree:equalBT(C,D2)),
  ?assertNot(splaytree:equalBT(D,B2)),
  ?assertNot(splaytree:equalBT(E,A2)).

isBT_test() ->
  {A,B,C,D,E}=validSplayTrees(),
  {AI,BI,CI,DI}=nonvalidSplayTrees(),
  ?assert(splaytree:isBT(A)),
  ?assert(splaytree:isBT(B)),
  ?assert(splaytree:isBT(C)),
  ?assert(splaytree:isBT(D)),
  ?assert(splaytree:isBT(E)),
  ?assertNot(splaytree:isBT(AI)),
  ?assertNot(splaytree:isBT(BI)),
  ?assertNot(splaytree:isBT(CI)),
  ?assertNot(splaytree:isBT(DI)).

insertBT_test() -> true.
deleteBT_test() -> true.

findSBT_test() ->
  {A,B,C,D,E}=validSplayTrees(),
  %Going down the left tree
  ?assertEqual(3,splaytree:findSBT(A,5)),
  %Going down the right tree
  ?assertEqual(3,splaytree:findSBT(A,15)),
  %Finding the root
  ?assertEqual(4,splaytree:findSBT(A,10)),
  %This element is not in the tree
  ?assertEqual(-1,splaytree:findSBT(A,6)),
  %Left then right and find a leaf
  ?assertEqual(1,splaytree:findSBT(A,9)),
  %Right then left and find a leaf
  ?assertEqual(1,splaytree:findSBT(A,11)),
  %Go right and find there is nothing there. Abort then
  ?assertEqual(-1,splaytree:findSBT(B,11)),
  %Go left and find there is nothing there, Abort then
  ?assertEqual(-1,splaytree:findSBT(C,8)),
  %Empty tree. Abort search
  ?assertEqual(-1,splaytree:findSBT(D,42)),
  %Tree has only one element.
  ?assertEqual(1,splaytree:findSBT(E,99)).

findBT_test() -> true.

findTP_test() ->
{A,B,C,D,E}=findSplayTrees(),
{ATP,BTP,BTPF,C14,C12,CF,D16,D18,DF,E17,E11,E10,E18,EF}=foundTPSplayTrees(),
?assertEqual(ATP,splaytree:findTP(A,99)),
?assertEqual(BTP,splaytree:findTP(B,10)),
?assertEqual(BTPF,splaytree:findTP(B,99)),
?assertEqual(C14,splaytree:findTP(C,14)),
?assertEqual(C12,splaytree:findTP(C,12)),
?assertEqual(CF,splaytree:findTP(C,99)),
?assertEqual(D16,splaytree:findTP(D,16)),
?assertEqual(D18,splaytree:findTP(D,18)),
?assertEqual(DF,splaytree:findTP(D,99)),
?assertEqual(E17,splaytree:findTP(E,17)),
?assertEqual(E11,splaytree:findTP(E11)),
?assertEqual(E10,splaytree:findTP(E10)),
?assertEqual(E18,splaytree:findTP(E18)),
?assertEqual(EF,splaytree:findTP(E,99)).


%%%%%%%%%%%%%%%%
%% TREES
%%%%%%%%%%%%%%%%
findSplayTrees() ->
%Empty Tree
A=nulltree,
%Tree with one element
B={10,1,nulltree,nulltree},
%Only left to search
C={15,4,
    {14,3,
      {13,2,
        {12,1,nulltree,nulltree},
        nulltree
      },
      nulltree
     },
    nulltree
   },
%Only right side to search
D={15,4,
    nulltree,
    {16,3,
      nulltree,
      {17,2,
        nulltree,
        {18,1,nulltree,nulltree}
      }
    }
  },
% Both sides to search
% Left-Right | Right-Left are possible
E={15,3,
    {10,2,
      {9,1,nulltree,nulltree},
      {11,1,nulltree,nulltree}
    },
    {18,2,
      {17,1,nulltree,nulltree},
      {19,1,nulltree,nulltree}
    }
  },
  {A,B,C,D,E}.

foundTPSplayTrees() ->
%Empty Tree stays empty. Nothing is found.
ATP={-1,nulltree},
%No rotation needed, there is only one element
BTP={1,{10,1,nulltree,nulltree}},
%Failcase for one element tree
BTPF={-1,{10,1,nulltree,nulltree}},
%14 is searched and transposed in Tree C
C14={3,{14,3,
      {13,2,
        {12,1,nulltree,nulltree},
        nulltree
      },
      {15,1,nulltree,nulltree}
    }},
%12 is searched and transposed in Tree C
C12={2,{15,4,
      {14,3,
        {12,2,
          nulltree,
          {13,1,nulltree,nulltree}
        },
        nulltree
      },
      nulltree
    }},
%Failcase for Tree C. Nothing is found
CF={-1,{15,4,
      {14,3,
      {13,2,
        {12,1,nulltree,nulltree},
        nulltree
      },
      nulltree
     },
    nulltree
   }},
%16 is found and transposed in Tree D
D16={3,{16,3,
      {15,1,nulltree,nulltree},
      {17,2,
        nulltree,
        {18,1,nulltree,nulltree}
      }
    }},
%18 is found and transposed in Tree D
D18={2,{15,4,
    nulltree,
      {16,3,
        nulltree,
        {18,2,
          {17,1,nulltree,nulltree},
          nulltree
        }
      }
    }},
%Failcase for Tree D. Nothing is found
DF={-1,{15,4,
    nulltree,
    {16,3,
      nulltree,
      {17,2,
        nulltree,
        {18,1,nulltree,nulltree}
      }
    }
  }},
%17 is found and transposed in Tree E (RL to Leaf)
E17={3,
    {15,4,
      {10,2,
        {9,1,nulltree,nulltree},
        {11,1,nulltree,nulltree}
      },
      {17,3,
        nulltree,
        {18,2,
          nulltree,
          {19,1,nulltree,nulltree}
        }
      }
    }},
%11 is found and transposed in Tree E (LR to leaf)
E11={3,{15,4,
      {11,3,
        {10,2,
          {9,1,nulltree,nulltree},
          nulltree
        },
       nulltree
      },
      {18,2,
        {17,1,nulltree,nulltree},
        {19,1,nulltree,nulltree}
      }
    }},
%10 is found and transposed in Tree E(Node found and complex rotate R)
E10={4,{10,4,
      {9,1,nulltree,nulltree},
      {15,3,
        {11,1,nulltree,nulltree},
        {18,2,
          {17,1,nulltree,nulltree},
          {19,1,nulltree,nulltree}
        }
      }
    }},
%18 is found and transposed in Tree E(Node found and complex rotate L)
E18={4,{18,4,
      {15,3,
        {10,2,
          {9,1,nulltree,nulltree},
          {11,1,nulltree,nulltree}
        },
        {17,1,nulltree,nulltree}
      },
      {19,1,nulltree,nulltree}
    }},
%Failcase for Tree E. Nothing is found
EF={-1,{15,3,
    {10,2,
      {9,1,nulltree,nulltree},
      {11,1,nulltree,nulltree}
    },
    {18,2,
      {17,1,nulltree,nulltree},
      {19,1,nulltree,nulltree}
    }
  }},
{ATP,BTP,BTPF,C14,C12,CF,D16,D18,DF,E17,E11,E10,E18,EF}.

validSplayTrees() ->
A={10,4,
    {5,3,
      {3,2,
        {1,1,nulltree,nulltree},
        {4,1,nulltree,nulltree}
      },
      {8,2,
        {7,1,nulltree,nulltree},
        {9,1,nulltree,nulltree}
      }
     },
     {15,3,
      {12,2,
        {11,1,nulltree,nulltree},
        {13,1,nulltree,nulltree}
      },
      {18,2,
        {17,1,nulltree,nulltree},
        {19,1,nulltree,nulltree}
      }
     }
    },

%Leftsided Tree.
B={10,4,
    {7,3,
      {5,2,
        {3,1,nulltree,nulltree},nulltree
      },nulltree
    },nulltree
  },

C={10,4,
    nulltree,
    {11,3,
      nulltree,
      {12,2,
        nulltree,
          {15,1,nulltree,nulltree}
       }
      }
    },
%Empty Tree is also valid
D=nulltree,
%Only a leaf is also valid
E={99,1,nulltree,nulltree},
{A,B,C,D,E}.

nonvalidSplayTrees() ->
% Non-Valid sized elements right side
A={10,4,
    nulltree,
    {9,3,
      nulltree,
      {8,2,
        nulltree,
          {7,1,nulltree,nulltree}
       }
      }
    },

% Non-Valid sized elements left side
B={10,4,
    {12,3,
      {15,2,
        {18,1,nulltree,nulltree},nulltree
      },nulltree
    },nulltree
  },

% Non valid heights on valid sized elements.
C={10,4,
   {5,3,
    {3,1,
      {1,1,nulltree,nulltree},
      {4,1,nulltree,nulltree}
    },
    {8,1,
      {7,1,nulltree,nulltree},
      {9,1,nulltree,nulltree}
    }
   },
   {15,3,
    {12,1,
      {11,1,nulltree,nulltree},
      {13,1,nulltree,nulltree}
    },
    {18,1,
      {17,1,nulltree,nulltree},
      {19,1,nulltree,nulltree}
    }
   }
  },
D=noTree,
{A,B,C,D}.
