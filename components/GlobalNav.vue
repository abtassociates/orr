<template>
  <div>
    <div class="hud-header"></div>
    <div class="hud-nav-header flex">
      <div class="hud-nav-image flex-none">
        <a :href="$config.public.hdxBase">
          <img src="/img/hdx_logo_nav_white.png" alt="DHX Homelessness Data Exchange 2.0" />
        </a>
      </div>
      <div class="flex-1 grid">
        <div class="hud-nav flex justify-self-end">
          <a class="nav-item nav-link" :href="$config.public.hdxBase + '/home'">Home</a>
          <a class="nav-item nav-link" :href="$config.public.hdxBase + '/sitestatus?module=lsa'">LSA</a>
          <a class="nav-item nav-link" :href="$config.public.hicBase">HIC</a>
          <a class="nav-item nav-link" :href="$config.public.pitBase">PIT</a>
          <a class="nav-item nav-link" :href="$config.public.spmBase">SPM</a>
          <a class="nav-item nav-link active" href="/">ORR<span class="sr-only">(current)</span></a>
          <a class="nav-item nav-link" :href="$config.public.stpBase">Stella P</a>
          <a class="nav-item nav-link" :href="$config.public.stmBase">Stella M</a>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.hud-header {
  background-color: #003366;
  height: 40px;
}

.hud-nav-header {
  background-color: #175892;
}

.hud-nav-image {
  padding: 10px 30px;

  img {
    height: 30px;
  }
}

.hud-nav {
  height: 50px;
  padding: 0 30px 0 0;
}

.nav-item {
  padding: 14px 20px 0 20px;
}

.nav-link {
  color: #fff;
  font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
  font-size: 14px;
  font-weight: 700;
}

.nav-link.active {
  color: #175892;
  background-color: #ffffff;
}

.nav-link:hover {
  color: #0a1e54;
  background-color: #dae8ea;
}
</style>
