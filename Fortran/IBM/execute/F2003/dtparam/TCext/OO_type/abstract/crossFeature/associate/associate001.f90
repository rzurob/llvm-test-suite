! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/associate/associate001.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate001.f
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
!*                                         a) Associate-name associating with scalar variable(s)
!*                                            1) variable being an abstract type scalar
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

   integer function baseid(a)
      class(base(4)), intent(in) :: a
      baseid = a%id
   end function

end module

program associate001
   use m

   class(base(4)), pointer :: b1
   class(base(4)), allocatable, target :: b2

   allocate (b2, source = child(4,4,20)(5))
   allocate (b1, source = b2)

   associate ( pointer => b1, allocatable => b2 )
      if ( pointer%type() .ne. 2 )               error stop 1_4
      if ( pointer%getid() .ne. 5 )              error stop 2_4
      if ( allocatable%type() .ne. 2 )           error stop 3_4
      if ( allocatable%getid() .ne. 5 )          error stop 4_4
      b1%id = 0
      if ( pointer%getid() .ne. 0 )              error stop 5_4
      if ( allocatable%getid() .ne. 5 )          error stop 6_4
   end associate

end program
