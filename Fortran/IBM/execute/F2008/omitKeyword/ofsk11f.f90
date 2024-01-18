!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : ofsk11f.f
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  Internal recursive subroutine is terminated by "END"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      PROGRAM fxre0001
C***********************************************************************
C*
C*   INITIAL DECLARATIONS
C*
C***********************************************************************

      INTEGER CASENUM,i,j
      INTEGER lngth,place
      character*10 :: alphabet(10)
      character*10 :: ch(10,10), check(10,10)
      character*10 :: cha(10,10)
      AUTOMATIC cha

      alphabet(1) = 'aaaaaaaaaa'
      alphabet(2) = 'bbbbbbbbbb'
      alphabet(3) = 'cccccccccc'
      alphabet(4) = 'dddddddddd'
      alphabet(5) = 'eeeeeeeeee'
      alphabet(6) = 'ffffffffff'
      alphabet(7) = 'gggggggggg'
      alphabet(8) = 'hhhhhhhhhh'
      alphabet(9) = 'iiiiiiiiii'
      alphabet(10) = 'jjjjjjjjjj'

      check(5,:) = 'abcdeedcba'
      check(6,:) = 'abcdeedcba'
      check(:,5) = 'abcdeedcba'
      check(:,6) = 'abcdeedcba'
      check(4,:) = 'abcddddcba'
      check(7,:) = 'abcddddcba'
      check(:,4) = 'abcddddcba'
      check(:,7) = 'abcddddcba'
      check(3,:) = 'abccccccba'
      check(8,:) = 'abccccccba'
      check(:,3) = 'abccccccba'
      check(:,8) = 'abccccccba'
      check(2,:) = 'abbbbbbbba'
      check(9,:) = 'abbbbbbbba'
      check(:,2) = 'abbbbbbbba'
      check(:,9) = 'abbbbbbbba'
      check(1,:) = 'aaaaaaaaaa'
      check(10,:) = 'aaaaaaaaaa'
      check(:,1) = 'aaaaaaaaaa'
      check(:,10) = 'aaaaaaaaaa'

      CASENUM = 001
      place = 1
      lngth = 10
      ch = ' '
      call sub1(ch)
      do i=1,10
         do j = 1,10

            if ( check(i,j) .NE. ch(i,j) )
     +         CALL ZZRC( CASENUM )
         enddo
      enddo

      CASENUM = 002
      place = 1
      lngth = 10
      ch = ' '
      call sub1(cha)
      print *,cha
      do i=1,10
         do j = 1,10

            if ( check(i,j) .NE. cha(i,j) )
     +         CALL ZZRC( CASENUM )
         enddo
      enddo

      contains


C***********************************************************************
C*
C*  This routine Creates a matrix of character elements where
C*  the outside rows starts with all a's and then adds at each internal
C*  row and column the next consecutive letter.
C*
C* ie. (6x6)      aaaaaa aaaaaa aaaaaa aaaaaa aaaaaa aaaaaa
C*                aaaaaa abbbba abbbba abbbba abbbba aaaaaa
C*                aaaaaa abbbba abccba abccba abbbba aaaaaa
C*                aaaaaa abbbba abccba abccba abbbba aaaaaa
C*                aaaaaa abbbba abbbba abbbba abbbba aaaaaa
C*                aaaaaa aaaaaa aaaaaa aaaaaa aaaaaa aaaaaa
C*
C***********************************************************************

      RECURSIVE SUBROUTINE SUB1( Arg1 )
         implicit none
         character (len=lngth) :: Arg1(:,:)
         integer u1,u2

         u1 = ubound(Arg1,1)
         u2 = ubound(Arg1,2)

         Arg1(1:u1,1:u2)(1:lngth) = alphabet(place)
         if (
     +   ( u1 .GT. 1 ) .AND.
     +   ( u2 .GT. 1 ) .AND.
     +   ( lngth .GT. 1 ) ) THEN
             place = place + 1
             u1 = u1 - 1
             u2 = u2 - 1
             lngth = lngth - 2
              call SUB1( Arg1(2:u1,2:u2)(2:lngth+1) )
          endif

      END

      end
