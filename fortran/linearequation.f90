!Copyright (C) 2007 Ashwith Jerome Rego
!
!This program is free software: you can redistribute it and/or modify
!it under the terms of the GNU General Public License as published by
!the Free Software Foundation, either version 3 of the License, or
!(at your option) any later version.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program.  If not, see <http://www.gnu.org/licenses/>.


PROGRAM LINEAR_EQUATION
  IMPLICIT NONE
  REAL, ALLOCATABLE, DIMENSION (:,:) :: Eqn
  REAL, ALLOCATABLE, DIMENSION(:) :: Soln
  INTEGER :: n, i, j, ErrFlag
    Soln = 0
  PRINT *, "Enter number of variables"
  READ*, n
  ALLOCATE(Eqn(n,n+1))
  ALLOCATE(Soln(n))
  
  PRINT*,"Enter equation coefficients..."
  PRINT*,"Equation should be of the form:"
  PRINT*,"a0 x0 + a1 x1 + a2 x2 +......+ an-1 xn-1 = an"
  DO i = 1, n
      READ*,(Eqn(i,j), j = 1, n+1)
  END  DO  
  CALL SOLVE(Eqn, Soln, n, ErrFlag)
  DO i =1,n
    PRINT*,"x",i," =", Soln(i)
  END DO
END PROGRAM LINEAR_EQUATION

!Subroutine to solve a set of simultaneous linear equations in 'n' variables
!using Gaussian Elimination method
!Author : Louisda16th a.k.a Ashwith J. Rego
!Description:
!This subroutine finds the unknowns in a set of linear equations
!An example of using Gaussian Elimination is as follows:
!Consider the equations:
!           8X2 + 2X3 = -7
!  3X1 + 5x2 + 2X3 = 8
!  6X1 + 2X2 + 8X3 = 26
! Write the equations in matrix form as   0 8 2 -7
!                                           3 5 2 8
!                                           6 2 8 26
! Re-order the equations as 6 2 8 26   
!                           3 5 2 8
!                           0 8 2 -7
! Now Convert to uppper traingular form to get   6 2 8 26
!                                                0 8 2 -7
!                                                0 0 -3 -3/2
! Finally use back-substitution to solve the equation. For example, in case of x3 you get:
!                                                          -3X3 = -3/2
!                                                     =>     X3 = -1/2
!For a detailed explanation on the algorithm check out http://en.wikipedia.org/wiki/Gaussian_elimination
!Reference : The program has been written entirely by me. However, the algorithm is from Advanced Engineering Mathematics by Erwin Kreyzing, 8th Edition, Wiley              
!                                                      
SUBROUTINE SOLVE(a, x, n, errflag)
!Declarations
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n   !Stores the number of unknowns
  INTEGER, INTENT(OUT) :: errflag
  REAL, DIMENSION(n,n+1) :: a!An n x n+1 matrix which stores the simultaneous equations
  REAL, INTENT(OUT), DIMENSION(n) :: x !Array to store the solutions
  INTEGER :: i,j,k !Counters for Loops
  INTEGER :: largest
  REAL :: temp, m, sums
  LOGICAL :: FLAG
  m = 0
  !Solve Using Gaussian Elimination
  FLAG = .FALSE.
  x = 0
  DO k = 1, n-1
    DO j = 1, n
      IF (a(j, 1) /= 0 ) FLAG = .TRUE.
    END DO
    IF (FLAG .EQV. .FALSE.) THEN
      PRINT*,"No Unique Solution"
      errflag = -1
      x = 0
      EXIT
    ELSE
      largest = k
      !Find largest coefficient of first unknown
      DO j = k, n
        IF (ABS(a(j, k)) > ABS(a(largest,k))) largest = j
      END DO      
      !Make the equation with largest first coefficient as the first equation
      !Largest coefficient is chosen to prevent round-off errors as far as possible
      DO j = 1, n + 1
        temp = a(k, j)
        a(k,j) = a(largest,j)
        a(largest,j)=temp
      END DO
    ENDIF
    !Convert the input matrix to Upper Traingualar form
    DO j = k+1, n
      m = a(j,k)/a(k,k)
      DO i = k+1, n+1
        a(j,i) = a(j,i) - m*a(k,i)
      END DO
    END DO
    !No unique solution exists if the last element in the upper triangular matrix is zero
    IF (a(n,n) == 0) THEN
      PRINT*,"No Unique Solution"
      errflag = -1
      x = 0
      EXIT
    ELSE
      !Find xn
      x(n) = a(n,n+1)/a(n,n)
      !Find the remaining unknowns by back-substitution
      DO i = n-1, 1, -1
        sums = 0
        DO j = i+1, n
          sums = sums + a(i,j)*x(j)
        END DO
        x(i) = (a(i,n+1) - sums)/a(i,i)
      END DO
    ENDIF
  END DO
  
END SUBROUTINE SOLVE
    
