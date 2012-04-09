<apply template="page">
  <h1>Logg inn</h1>
  <form action="/login/${refpage}" method="post">
    Brukernavn: <input type="text" size="20" name="login" value="" /><br />
    Passord: <input type="password" size="20" name="password" value="" /><br />
    <input type="hidden" name="referrer" value="/${refpage}" /><br />
    <input type="submit" value="Logg inn" />
  </form>
</apply>
