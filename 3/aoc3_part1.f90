program aoc3_part1
  implicit none
  character(1) :: input(140, 140), mask(140, 140)
  integer :: stat, answer

  answer = 0

  block
    integer :: i, j
    character(140) :: line
    open(10, file='input.txt', status='old', iostat=stat)
    do i=1,140
        read(10, '(A)') line
        do j=1,140
          input(i,j) = line(j:j)
        end do
    end do
    close(10)
  end block

  where (is_number(input))
    mask = 'n'
  else where (is_symbol(input))
    mask = 's' 
  else where
    mask = input
  end where

  where (mask(1:139, 1:139) == 's' .and. mask(2:140, 2:140) == 'n')
    mask(2:140, 2:140) = 'm'
  end where
  where (mask(2:140, 2:140) == 's' .and. mask(1:139, 1:139) == 'n')
    mask(1:139, 1:139) = 'm'
  end where
  where (mask(2:140, 1:139) == 's' .and. mask(1:139, 2:140) == 'n')
    mask(1:139, 2:140) = 'm'
  end where
  where (mask(1:139, 2:140) == 's' .and. mask(2:140, 1:139) == 'n')
    mask(2:140, 1:139) = 'm'
  end where

  where (mask(1:139, 1:140) == 's' .and. mask(2:140, 1:140) == 'n')
    mask(2:140, 1:140) = 'm'
  end where
  where (mask(1:140, 1:139) == 's' .and. mask(1:140, 2:140) == 'n')
    mask(1:140, 2:140) = 'm'
  end where
  where (mask(2:140, 1:140) == 's' .and. mask(1:139, 1:140) == 'n')
    mask(1:139, 1:140) = 'm'
  end where
  where (mask(1:140, 2:140) == 's' .and. mask(1:140, 1:139) == 'n')
    mask(1:140, 1:139) = 'm'
  end where

  block
    integer :: i, j
    logical :: flood

    flood = .false.

    do i=1,140
      do j=1,140

        if (mask(i,j) /= 'n') then
          flood = mask(i,j) == 'm'
        end if

        if (flood) mask(i,j) = 'm'
      end do
      do j=140,1,-1

        if (mask(i,j) /= 'n') then
          flood = mask(i,j) == 'm'
        end if

        if (flood) mask(i,j) = 'm'
      end do
    end do
  end block

  where (mask == 'm')
    input = input
  else where
    input = '.'
  end where

  block
    integer :: i, j, k, n_numbers,n
    logical :: dots_started, first
    character(10) :: numbers

    numbers = '          '

    k = 1
    n_numbers=0
    do i=1,140
      first = .true.
      dots_started=is_number(input(i,j))
      do j=1,140
        if (is_number(input(i,j))) then
          dots_started = .false.
          numbers(k:k) = input(i,j)
          first=.false.
          k=k+1
!          print *, numbers
        else if (input(i,j) == '.' .and. .not. dots_started) then
          dots_started = .true.
          read(numbers, '(I10)') n
!          print *, "numbers", numbers, "n", n
          n_numbers=n_numbers+1
          k=1
          numbers = '          '
          answer = answer + n
        end if
      end do
    end do
    numbers(k+1:k+10)='----------'


  end block

  print *, answer

  contains

    logical elemental function is_symbol(ch)
      character(1), intent(in) :: ch
      is_symbol = ch > '9' .or. ch < '0' .and. .not. ch == '.'
    end function

    logical elemental function is_number(ch)
      character(1), intent(in) :: ch
      is_number = ch .le. '9' .and. ch .ge. '0'
    end function


end program aoc3_part1
