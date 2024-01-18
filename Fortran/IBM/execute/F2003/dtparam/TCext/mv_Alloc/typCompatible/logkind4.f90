! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/typCompatible/logkind4.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : logkind4.f
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
!*  DESCRIPTION                : FROM is of logical, TO of unlimit poly 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type base(k1,n1)    ! (4,20)
          integer, kind :: k1
          integer, len  :: n1
      end type

      type, extends(base) :: child    ! (4,20)
      end type

      class(*), allocatable ::  a1,a2
      type(base(4,20)) b1
      type(child(4,20)) c1

      logical, allocatable :: l1(:,:)
      class(*), allocatable :: o(:,:)

      allocate ( l1(2,2), source= reshape ( (/ extends_type_of(a1,a2), &
                     extends_type_of(a2, b1), extends_type_of(c1, b1), &
                     extends_type_of(b1,c1) /), (/2,2/) ) )

       call move_alloc(l1, o)

       if ( .not. allocated(o) ) stop 21
       if ( allocated(l1) ) stop 23

       print *, shape(o)

       select type (o)
              type is (logical)
                  print *, o
              class default
                  stop 21
       end select
       end 
