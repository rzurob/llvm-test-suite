! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/associate/associate002.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate002.f
! %VERIFY: associate002.out:associate002.vf
! %STDIN:
! %STDOUT: associate002.out
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
!*                                         b) Associate-name associating with array variable(s)
!*                                            1) variable being an abstract type whole array
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
      procedure, pass :: getid => baseid
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

   integer elemental function baseid(a)
      class(base(4)), intent(in) :: a
      baseid = a%id
   end function

end module

program associate002
   use m

   class(base(4)), allocatable :: b1(:)
   class(base(4)), pointer :: b2(:)
   allocate (b1(2), source = (/(child(4,4,20)(i),i=8,9) /) )
   allocate (b2(2), source = (/(child(4,4,20)(i),i=10,11) /) )

   associate ( myb1 => b1 , myb2 => b2)

      if ( myb1%type() .ne. 2 )    error stop 1_4
      if ( myb2%type() .ne. 2 )    error stop 2_4

      print *, myb1%getid()
      print *, myb2%getid()

      b1%id = 0

      if ( myb1%type() .ne. 2 )    error stop 3_4
      if ( myb2%type() .ne. 2 )    error stop 4_4

      print *, myb1%getid()
      print *, myb2%getid()

   end associate

   print *, b1%getid()
   print *, b2%getid()

end program
