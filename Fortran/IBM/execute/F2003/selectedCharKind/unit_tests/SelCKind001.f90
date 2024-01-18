!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : SelCKind001.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Zheming Gu
!*  DATE                       : August 02, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic function selected_char_kind(name)
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 317648
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test return values that equals to the kind type parameter of
!*  the character type specified by the argument NAME if the processor
!*  supports such a type, otherwise returns value -1. XL supports "ASCII" and "DEFAULT" 
!*  which means return value should be 1 if NAME is "ASCII" or "DEFAULT",otherwise returns -1     
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program SelCKind001
      
        character(5),parameter :: c = "ASCII"	
        character(7),parameter :: d = "DEFAULT"
        
	integer :: i = selected_char_kind(c)
	integer :: j = selected_char_kind("ASCII")
        integer :: k = selected_char_kind(d)
        integer :: m = selected_char_kind("DEFAULT")
        
	print *,i   
	print *,j
        print *,k
        print *,m
        
        print *,selected_char_kind("DefAULt")     ! mixed case test

        print *,selected_char_kind("nothing")     ! not supported type

        print *,selected_char_kind("default")     ! lower-case test

        print *,selected_char_kind("asciiascii")  

        print *,selected_char_kind(" ascii")      ! heading space test

        print *,selected_char_kind("default ")    ! tailing space test
        
        
end


