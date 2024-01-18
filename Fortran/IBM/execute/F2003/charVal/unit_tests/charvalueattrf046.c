//**********************************************************************
// %START
// %MAIN: YES
// %PRECMD:
// %COMPOPTS: 
// %GROUP: charvalueattrf046.f
// %VERIFY:
// %STDIN:
// %STDOUT:
// %EXECARGS:
// %POSTCMD:
// %END
//**********************************************************************
//*  ===================================================================
//*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
//*  ===================================================================
//*
//*  TEST CASE TITLE            : charvalueattrf046
//*
//*  PROGRAMMER                 : Bardia Mahjour
//*  DATE                       : Jan. 25, 2006
//*  ORIGIN                     : AIX Compiler Development,
//*                             : IBM Software Solutions Toronto Lab
//*
//*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the VALUE
//*                               attribute when used with characters of 
//*                               length other than 1. ( Feature 298120 )   
//*                                                   
//*  SECONDARY FUNCTIONS TESTED : None 
//*
//*  DRIVER STANZA              : xlc
//*  REQUIRED COMPILER OPTIONS  : 
//*
//*  DESCRIPTION                : Test interoperability of characters with
//*                               length 1 that have VALUE attr. with C.
//*
//234567890123456789012345678901234567890123456789012345678901234567890
void s1(char c)
{
  printf("%c\n",c);
  c = 'x';
  return;
}

void s2(char* c)
{
  printf("%c\n",*c);
  *c = 'z';
  return;
}
