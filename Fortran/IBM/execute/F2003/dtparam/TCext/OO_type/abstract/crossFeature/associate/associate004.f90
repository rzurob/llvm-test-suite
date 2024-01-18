! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/associate/associate004.f
!######################################################################
! SCCS ID Information                                                  
! %W%, %I%                                                             
! Extract Date/Time: %D% %T%                                           
! Checkin Date/Time: %E% %U%                                           
!######################################################################
! *********************************************************************
! %START                                                               
! %MAIN: YES                                                           
! %PRECMD: rm -f *.mod                                                 
! %COMPOPTS: -qfree=f90                                                
! %GROUP: redherring.f                                                   
! %VERIFY:                                     
! %STDIN:                                                              
! %STDOUT: 
! %EXECARGS:                                                           
! %POSTCMD: dcomp associate004.f                                                            
! %END                                                                 
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing:  Associate Construct
!*                                         b) Associate-name associating with array variable(s) (array section with vector subscripts)
!*                                            1) variable being an abstract type object (pointer)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m
   
   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   contains
      procedure, nopass :: type => basetype
   end type
   
   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   contains
      procedure, nopass :: type => childtype
   end type

contains

   integer function basetype()
      basetype = 1
   end function

   integer function childtype()
      childtype = 2
   end function
end module

program associate004
   use m
   class(base(4)), pointer :: b1(:)
   class(base(4)), allocatable, target :: b2(:)
   
   allocate (b2(3), source = (/child(4,4,20)(8),child(4,4,20)(10),child(4,4,20)(9)/) )
   
   b1 => b2

   associate ( myb1 => b1((/1,3/)), myb2 => b2((/3,2/)) )
      if ( myb1%type() .ne. 2 )         error stop 1_4
      if ( myb2%type() .ne. 2 )         error stop 2_4
      print *, myb1%id
      print *, myb2%id
   end associate
   
end program
