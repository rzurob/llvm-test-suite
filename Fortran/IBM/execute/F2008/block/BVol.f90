!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : BVol
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : var declared in containing scope marked as volatile in block
!*
!*  DESCRIPTION
!*
!*  There are two things to verify here: 1. that the variables are not redeclared
!*  in the block, and 2. that the operations on those variables within the BLOCK
!*  are, in fact, appropriate for volatile variables.  The latter we will verify
!*  by examining the generated WCODE, making sure that any LOD, STR, STO and IND
!*  directives associated with the variables in the BLOCK are marked as "volatile".
!*  We can also introduce new non-volatile variables in the BLOCK to verify that
!*  they are *not* marked volatile.
!*  To analyse the WCODE: the only volatile references in the program should be
!*  references to "[dis][12]obvious" in the BLOCK, and all references there to
!*  "[dis][12]obvious" must be volatile.  With one exception, everything else
!*  should be non-volatile.  The lone exception is d2obvious, which is implicitly
!*  declared in the block as volatile, and only appears in the block; oddly, that
!*  means that it is volatile for its entire extent in the program, which means
!*  that it will be marked volatile on the SYM, and also appear as "Volatile" in
!*  the attribute listing.  No other variable should appear as "Volatile" there.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BVol

  implicit integer(4) (i), real(8) (d), character(6) (s)

  ! we declare these initially with a specific type not in accord with
  ! the implicit statement so that we can verify in the BLOCK that they
  ! have the expected non-implicit type.
  integer(4)   :: d0obvious
  real(4)      :: i1obvious
  character(4) :: s1obvious ! len here is 4, different from implicit declaration

  ! None of the following references should be marked volatile by the compiler:
  d0obvious = 1234567
  i1obvious = 6.8878e9
  s1obvious = 'Testing'
  ! d2obvious is declared implicitly
  d2obvious = 3.14159265358979323_8

  print *, trim(identifyType('o.d1o',d0obvious))
  print *, trim(identifyType('o.i1o',i1obvious))
  print *, trim(identifyType('o.s1o',s1obvious))
  print *, trim(identifyType('o.d2o',d2obvious))

  block ! Any time you change the line number of this line, you must update BVol.awk

    ! All but one (d0obvious) of the above are now made volatile,
    ! and s2obvious is now introduced as a volatile variable of implicit type.
    ! s0obvious is also introduced with implicit type, but as a non-volatile array.
    volatile :: i1obvious, s1obvious, d2obvious, s2obvious
    dimension :: s0obvious(1,2)

    ! s0obvious and d0obvious are *not* volatile, but everything else should be.
    d0obvious = d0obvious + 7654321
    d2obvious = d2obvious * 2.71828182845904523536028747135266249775724709369995_8
    i1obvious = i1obvious / 196.8e-3
    s1obvious = s1obvious(2:3) // 'Gnitset'
    s2obvious = 'Snorri'
    s0obvious = 'extras'

    print *, trim(identifyType('i.d1o',d0obvious))
    print *, trim(identifyType('i.d2o',d2obvious))
    print *, trim(identifyType('i.i1o',i1obvious))
    print *, trim(identifyType('i.s1o',s1obvious))
    print *, trim(identifyType('i.s2o',s2obvious))
    print *, identifyType('i.s3o',s0obvious)

  end block ! Any time you change the line number of this line, you must update BVol.awk

  ! None of these should be volatile:
  d0obvious = d0obvious - 7654321
  i1obvious = i1obvious * 196.8e-3
  s1obvious = s1obvious(2:3) // 'snurt'
  d2obvious = d2obvious / 2.71828182845904523536028747135266249775724709369995_8

  print *, trim(identifyType('x.d1o',d0obvious))
  print *, trim(identifyType('x.i1o',i1obvious))
  print *, trim(identifyType('x.s1o',s1obvious))
  print *, trim(identifyType('x.d2o',d2obvious))

  print *, d0obvious, i1obvious, s1obvious, d2obvious

contains

  elemental character(200) function identifyType(label,arg)
    character(4), intent(in) :: label
    class(*), intent(in) :: arg
    select type(arg)
      type is (integer(2));   write(identifyType,*) label, '.i2', kind(arg), arg
      type is (integer(4));   write(identifyType,*) label, '.i4', kind(arg), arg
      type is (real(4));      write(identifyType,*) label, '.r4', kind(arg), arg
      type is (real(8));      write(identifyType,*) label, '.r8', kind(arg), arg
      type is (complex(4));   write(identifyType,*) label, '.z4', kind(arg), arg
      type is (complex(8));   write(identifyType,*) label, '.z8', kind(arg), arg
      type is (character(*)); write(identifyType,*) label, '.c', len(arg), '>', arg, '<'
      class default;          write(identifyType,*) label, ' unknown'
    end select
  end function

end program BVol
