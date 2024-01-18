! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/associate/associate008.f
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
! %GROUP: associate008.f                                                  
! %VERIFY: associate008.out:associate008.vf
! %STDIN:                                                              
! %STDOUT: associate008.out
! %EXECARGS:                                                           
! %POSTCMD:                                                  
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
!*                               e) Associate-name associating with type components
!*                                  1) multiple level of associate construct abstract polymorphic array section as selector
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type :: data(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
   contains
      procedure, pass :: getid
   end type
   
   type, abstract :: base(k2,n1)    ! (4,20)
      integer, kind  :: k2
      integer, len   :: n1
      type(data(k2)) :: id
   contains
      procedure, nopass :: type => basetype
   end type
   
   type, extends(base) :: child(k3,n2)    ! (4,20,4,20)
       integer, kind :: k3
       integer, len  :: n2
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
   
   integer elemental function getid(a)
      class(data(4)), intent(in) :: a
      getid = a%i
   end function
   
end module

program associate008
   use m
   
   class(base(4,20)), allocatable :: b1(:)
   allocate (b1(3), source = (/ child(4,20,4,20)(data(4)(5)), child(4,20,4,20)(data(4)(6)), child(4,20,4,20)(data(4)(7)) /) )  
   
   associate ( myb1 => b1(1:3:2)%id, myb2 => b1(2:3:2)%id, myb3 => b1(3:1:-2)%id, myb4 => b1((/1,2,3/))%id )
      
      print *,myb1%getid()
      print *,myb2%getid()
      print *,myb3%getid()
      print *,myb4%getid()     
      
      associate ( myb11 => myb1%i )
         print *, myb11
      end associate 
   end associate
   
   print *, b1%id%i
   
end program

