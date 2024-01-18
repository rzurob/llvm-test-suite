!* =================================================================== &
!*
!* DATE                       : March 10, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Intrinsic types in TYPE spec
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              Intrinsic types in TYPE spec
!*                              in allocate, ac and select type
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program intrinsictype03d

      class(*),allocatable :: p1

      type(integer), allocatable :: ta1 (:)

      type(integer), allocatable :: tb1 (:)
      type(real), allocatable :: tb2 (:)
      type(complex), allocatable :: tb3 (:)
      type(character(3)), allocatable :: tb4 (:)
      type(logical), allocatable :: tb5 (:)

      allocate ( type(integer) :: tb1(2) )
      allocate ( type(real) :: tb2(2) )
      allocate ( type(complex) :: tb3(2) )
      allocate ( type(character(3)) :: tb4(2) )
      allocate ( type(logical) :: tb5(2) )

      tb1 = [ type(integer) :: 1, 2 ]
      tb2 = [ type(real) :: 1.0, 2.0 ]
      tb3 = [ type(complex) :: (1.0,1.0), (2.0,2.0) ]
      tb4 = [ type(character(3)) :: "abc", "def" ]
      tb5 = [ type(logical) :: .true., .false. ]

      allocate ( p1, source = ta1(1) )

      select type (p1)
      type is ( type(integer) )
        if ( p1 .NE. 1 ) then; STOP 1; endif
        print *,"integer"
      type is ( type(real) )
        if ( p1 .NE. 2.0 ) then; STOP 2; endif
        print *,"real"
      type is ( type(complex) )
        if ( real(p1) .NE. 3.0 ) then; STOP 3; endif
        print *,"complex"
      type is ( type(character(*)) )
        if ( p1 .NE. "A" ) then; STOP 4; endif
        print *,"character"
      type is ( type(logical) )
        if ( p1 .NEQV. .TRUE. ) then; STOP 5; endif
        print *,"logical"
      end select

      end
