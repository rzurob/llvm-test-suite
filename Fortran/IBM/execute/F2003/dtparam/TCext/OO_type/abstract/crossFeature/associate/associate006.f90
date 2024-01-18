! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/associate/associate006.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate006.f
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
!*                                  1) variable being an abstract type scalar with multiple level of associate construct
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
      integer, kind            :: k1
      integer(k1)              :: id
      class(base(k1)), pointer :: ptr =>null()
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

program associate006
   use m

   class(base(4)), allocatable, target  :: b1
   class(base(4)), pointer              :: b2
   class(base(4)), allocatable, target  :: c1

   allocate (c1, source = child(4,4,20)(9) )
   allocate (b1, source = child(4,4,20)(8,c1) )
   b2 => b1

   associate (myb2 => b2%ptr )
      if (myb2%type() .ne. 2)   error stop 1_4

      if ( myb2%id .ne. 9 ) error stop 2_4

      b2%ptr%id = 4

      if ( myb2%id .ne. 4 ) error stop 3_4

      associate ( b => myb2%id )
         b = 7
      end associate
   end associate

   if (b1%ptr%id .ne. 7 ) error stop 4_4

end program
