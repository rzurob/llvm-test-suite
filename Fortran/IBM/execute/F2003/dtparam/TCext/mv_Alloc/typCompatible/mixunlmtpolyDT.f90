! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodeferredlp /tstdev/F2003/mv_Alloc/typCompatible/mixunlmtpolyDT.f
! opt variations: -qnock -qnok -qnol -qdeferredlp

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : mixunlmtpolyDT.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : TO is of type unlimited poly
!*                               FROM is a non-poly DT 
!*                               deferred pointer char as component 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      type  :: base(k1,n1)    ! (4,20)
          integer, kind :: k1
          integer, len  :: n1
          character(:), pointer :: ch(:)
      end type 

      class(*), allocatable :: a 
      type(base(4,20)), allocatable :: b 
end module

      use m
      character(:), pointer :: ch(:)

      allocate(ch(2), source = (/ 'XYZ', 'UVW' /) )

      allocate(base(4,20) :: a)

      select type (a)
          type is (base(4,*))
               a = base(4,20)(ch)
      end select
     
      allocate(b)
      allocate( b%ch(3), source = (/ '  IBM   ', 'compiler', ' Fortran' /) )

      call move_alloc(b, a)

      if ( allocated(b) ) stop 11
      if ( .not. allocated(a) ) stop 31

      select type (a)
          type is (base(4,*))
              print *, a%ch   
          class default
              stop 23
      end select  

      end
