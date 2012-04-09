<apply template="page">
  <h1>Administrer ønskeliste</h1>
  <notification />
  <h2>Ønskeliste</h2>
  <bind tag="wishTableHeader">
    <tr><th>Hva</th><th>Bilde</th><th>Butikk</th><th>Antall</th><th>Slett</th><th>Handling</th></tr>
  </bind>
  <apply template="wishtable" />
  <h2>Sett inn ønske i ønskeliste</h2>
  <apply template="admin-insert" />
</apply>
