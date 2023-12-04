program aoc4_part1
  implicit none

  character(116) :: line
  integer :: stat, answer

  answer = 0

  open(10, file='input.txt', status='old', iostat=stat)

  block
    character(5) :: game
    character(1) :: tmp
    character(3) :: tmp2

    integer :: igame, wnum(10), numbers(25)
    integer :: i, j, n_winning, points

  do while (.true.)
    read (10, '(A5,I3,A1,10I3,A3,25I3)', end=99) game, igame, tmp, wnum, tmp2, numbers

    n_winning = 0
    points = 0
    do i=1,10
      do j=1,25
        if (wnum(i) == numbers(j)) n_winning = n_winning + 1
      end do
    end do
    points = 2**(n_winning-1)
    write(*, '(10I4)') wnum
    write(*, '(25I4)') numbers
    print *, "n_winning", n_winning
    print *, "points", points
    answer=answer+points
  end do

  end block
  99 continue

  print *, answer
end
