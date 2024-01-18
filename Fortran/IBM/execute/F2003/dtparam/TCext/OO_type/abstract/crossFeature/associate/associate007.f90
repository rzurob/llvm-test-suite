! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/associate/associate007.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate007.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  Associate Construct
!*                               e) Associate-name associating with type components
!*                                  1) multiple level of associate construct abstract polymorphic array as selector
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

   integer function getid(a)
      class(data(4)), intent(in) :: a
      getid = a%i
   end function
end module

program associate007
   use m

   class(base(4,20)), allocatable, target :: b1(:)
   allocate (b1(3), source = (/ child(4,20,4,20)(data(4)(5)), child(4,20,4,20)(data(4)(6)), child(4,20,4,20)(data(4)(7)) /) )

   associate (myb1 => b1%id )

      if ( size(myb1) .ne. size(b1%id) ) error stop 1_4
      if ( myb1(1)%getid() .ne. 5 ) error stop 2_4
      if ( myb1(2)%getid() .ne. 6 ) error stop 3_4
      if ( myb1(3)%getid() .ne. 7 ) error stop 4_4
      associate ( myb11 => myb1%i )
         if ( size(myb11) .ne. size(myb1%i) ) error stop 5_4
         if ( myb11(1) .ne. 5 ) error stop 6_4
         if ( myb11(2) .ne. 6 ) error stop 7_4
         if ( myb11(3) .ne. 7 ) error stop 8_4
      end associate

   end associate

end program

