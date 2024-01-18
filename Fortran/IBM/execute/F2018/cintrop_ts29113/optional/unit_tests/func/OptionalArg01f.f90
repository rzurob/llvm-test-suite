!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : OptionalArg01f
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : May 23, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop OPTIONAL argument
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from C
!*                               where the procedure is defined in
!*                               Fortran.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      subroutine sub(arg) bind(C)
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end
