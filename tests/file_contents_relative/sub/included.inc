package main

sub_here :: () -> str {
    return #file_contents "payload.txt";
}

sub_up :: () -> str {
    return #file_contents "../payload.txt";
}
