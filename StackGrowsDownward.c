static int stack_grows_downward(int counter, char *baseptr) {
  if (counter < 100) {
    return stack_grows_downward(counter + 1, baseptr);
  } 
  else {
    char tester;
    return ((baseptr - (&tester)) > 0);
  }
}

int main(int argc, char *argv[]) {
  char basechar;
  return (stack_grows_downward(0, &basechar) ? 1 : 0);
}
