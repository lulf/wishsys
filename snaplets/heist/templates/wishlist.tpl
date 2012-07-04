<apply template="page">
  <h1>Registrere kjøp</h1>
  <notification />
  <p>I kolonnen 'Gjenværende' vises det hvor mange av en gjenstand vi ønsker oss som fortsatt ikke har blitt kjøpt. For å registrere et kjøp: fyll inn hvor mange du har kjøpt av en gjenstand og trykk 'Registrer'. Siden blir da lastet på nytt og antall gjenværende blir oppdatert. Når gjenværende er 0, betyr det at vi i utgangspunktet ikke ønsker oss flere av denne gjenstanden.</p>
  <bind tag="wishTableHeader">
    <tr><th>Hva</th><th>Bilde</th><th>Butikk</th><th>Gjenværende</th><th>Registrere</th></tr>
  </bind>
  <apply template="wishtable" />
  <p>Et lite tips: Om du skulle være uheldig å fylle inn eller trykke feil (f.eks. at du registrerte 3 i stedet for 2), så kan du registrere en gang til med negativt tall (skriv inn -1, og trykk 'Registrer') for å rette opp feilen.</p>
</apply>
