<apply template="page">
  <h1>Administrer ønskeliste</h1>
  <h2>Ønskeliste</h2>
  <bind tag="wishTableHeader">
    <tr><th>Hva</th><th>Bilde</th><th>Butikk</th></tr>
  </bind>
  <apply template="wishlist" />
  <h2>Sett inn ønske i ønskeliste</h2>
  <apply template="admin-insert" />
</apply>