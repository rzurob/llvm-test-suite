!* =================================================================== &
!*
!* DATE                       : March 10, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Intrinsic types in TYPE spec
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              Intrinsic types in TYPE spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program intrinsictype01f

      class(*),allocatable :: p1

      ITYPE :: t1

      ITYPE, allocatable :: ta1 (:)
      ITYPE, target :: tt
      ITYPE, pointer :: tp

      t1 = IVAL
      tt = IVAL

      allocate ( ta1 (1) )
      deallocate (ta1)
      allocate ( ta1 (1) )

      tp => tt
      ta1 = [ IVAL ]

      allocate ( p1, source = ta1(1) )

      select type (p1)
      type is ( integer )
        if ( p1 .NE. 1 ) then; STOP 1; endif
        print *,"integer"
      type is ( real )
        if ( p1 .NE. 2.0 ) then; STOP 2; endif
        print *,"real"
      type is ( complex )
        if ( real(p1) .NE. 3.0 ) then; STOP 3; endif
        print *,"complex"
      type is ( character(*) )
        if ( p1 .NE. "A" ) then; STOP 4; endif
        print *,"character"
      type is ( logical )
        if ( p1 .NEQV. .TRUE. ) then; STOP 5; endif
        print *,"logical"
      end select

      deallocate (p1)
      allocate ( p1, source = tp )

      select type (p1)
      type is ( integer )
        if ( p1 .NE. 1 ) then; STOP 1; endif
        print *,"yes, an integer"
      type is ( real )
        if ( p1 .NE. 2.0 ) then; STOP 2; endif
        print *,"yes, a real"
      type is ( complex )
        if ( real(p1) .NE. 3.0 ) then; STOP 3; endif
        print *,"yes, a complex"
      type is ( character(*) )
        if ( p1 .NE. "A" ) then; STOP 4; endif
        print *,"yes, a character"
      type is ( logical )
        if ( p1 .NEQV. .TRUE. ) then; STOP 5; endif
        print *,"yes, a logical"
      end select
      end
