! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mv_Alloc/typCompatible/logkind8a.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of logical*8
!*                               To is of unlimited poly
!*                               defect 321816
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      @process intsize(8)

      implicit logical ( a-z )

      allocatable :: l1(:)

      allocate(l1(2), source = func())

      if ( l1(1) .neqv. .false. ) stop 21
      if ( l1(2) .neqv. .true. ) stop 23

      contains
            function func()
                implicit logical*8 ( a-z )
                dimension func(2)

                type base(k1)    ! (4)
                    integer, kind :: k1
                end type

                type, extends(base) :: child    ! (4)
                end type

                class(base(4)), allocatable ::  a1,a2
                class(child(4)), pointer :: p1
                class(*), allocatable :: l1(:)
                allocatable l2(:)

                allocate(child(4)::a1)
                allocate(p1)

                allocate( l2(2), source =(/ same_type_as(a1,a2), &
                        same_type_as(a1,p1) /) )

                call move_alloc(l2, l1)

		if ( allocated(l2) ) stop 21
 		if ( .not. allocated(l1) ) stop 23

                select type (l1)
                     type is (logical*8)
                        func = l1
                     class default
                        stop 51
                end select
            end function

end
