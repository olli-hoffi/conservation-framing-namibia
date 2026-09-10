'use strict';

// Called by main.js after preloader fades out
window.initRiver = function () {
  const canvas = document.getElementById('river-canvas');
  const label  = document.getElementById('river-label');
  if (!canvas) return;

  // Mobile: CSS gradient fallback, no WebGL
  if (window.innerWidth <= 768 || !window.WebGLRenderingContext) {
    initMobileRiver();
    return;
  }

  // ── Three.js scene ───────────────────────────────────────────
  const scene    = new THREE.Scene();
  const W        = canvas.clientWidth  || window.innerWidth;
  const H        = canvas.clientHeight || window.innerHeight * 0.35;

  const camera = new THREE.PerspectiveCamera(60, W / H, 0.1, 100);
  camera.position.set(0, 4, 8);
  camera.lookAt(0, 0, 0);

  const renderer = new THREE.WebGLRenderer({ canvas, antialias: true, alpha: true });
  renderer.setSize(W, H, false);
  renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));

  // Sandy ground beneath river
  const groundGeo = new THREE.PlaneGeometry(20, 10);
  groundGeo.rotateX(-Math.PI / 2);
  const groundMat = new THREE.MeshBasicMaterial({ color: new THREE.Color(0xC4A882) });
  scene.add(new THREE.Mesh(groundGeo, groundMat));

  // River plane
  const riverGeo = new THREE.PlaneGeometry(16, 8, 64, 32);
  riverGeo.rotateX(-Math.PI / 2);

  const riverMat = new THREE.ShaderMaterial({
    uniforms: {
      uTime:       { value: 0 },
      uDryness:    { value: 0 },
      uWaterColor: { value: new THREE.Color(0x4A8FA8) },
      uDryColor:   { value: new THREE.Color(0xC4A882) },
    },
    vertexShader: `
      uniform float uTime;
      uniform float uDryness;
      varying vec2 vUv;
      void main() {
        vUv = uv;
        vec3 pos = position;
        float wave = sin(pos.x * 3.0 + uTime * 2.0) * 0.05 * (1.0 - uDryness);
        pos.y += wave;
        gl_Position = projectionMatrix * modelViewMatrix * vec4(pos, 1.0);
      }
    `,
    fragmentShader: `
      uniform float uTime;
      uniform float uDryness;
      uniform vec3 uWaterColor;
      uniform vec3 uDryColor;
      varying vec2 vUv;
      void main() {
        float riverWidth = mix(0.7, 0.08, uDryness);
        float edge = abs(vUv.x - 0.5);
        float inRiver = smoothstep(riverWidth * 0.5, riverWidth * 0.4, edge);

        float shimmer = sin(vUv.y * 20.0 - uTime * 3.0) * 0.05 * (1.0 - uDryness);

        float crack = fract(vUv.x * 8.0 + vUv.y * 6.0);
        float crackLine = smoothstep(0.0, 0.02, crack) * uDryness;

        vec3 waterCol = mix(uWaterColor, uDryColor, uDryness) + shimmer;
        vec3 crackCol = waterCol * (1.0 - crackLine * 0.3);

        gl_FragColor = vec4(mix(uDryColor, crackCol, inRiver), 1.0);
      }
    `
  });

  scene.add(new THREE.Mesh(riverGeo, riverMat));
  scene.add(new THREE.AmbientLight(0xffffff, 0.9));

  // ── Scroll-driven dryness via GSAP ScrollTrigger ─────────────
  if (window.ScrollTrigger) {
    ScrollTrigger.create({
      trigger: document.body,
      start: 'top top',
      end: 'bottom bottom',
      scrub: true,
      onUpdate: (self) => {
        riverMat.uniforms.uDryness.value = self.progress;
        updateLabel(self.progress);
        fireRiverGA(self.progress);
      }
    });
  } else {
    // Fallback: native scroll
    window.addEventListener('scroll', () => {
      const total = document.body.scrollHeight - window.innerHeight;
      const p = total > 0 ? window.scrollY / total : 0;
      riverMat.uniforms.uDryness.value = p;
      updateLabel(p);
      fireRiverGA(p);
    }, { passive: true });
  }

  // ── Resize ───────────────────────────────────────────────────
  window.addEventListener('resize', () => {
    if (window.innerWidth <= 768) return;
    const w = canvas.clientWidth;
    const h = canvas.clientHeight;
    camera.aspect = w / h;
    camera.updateProjectionMatrix();
    renderer.setSize(w, h, false);
  });

  // ── Render loop ──────────────────────────────────────────────
  function animate() {
    requestAnimationFrame(animate);
    riverMat.uniforms.uTime.value = performance.now() / 1000;
    renderer.render(scene, camera);
  }
  animate();
};

// ── River state label ─────────────────────────────────────────
function updateLabel(p) {
  const label = document.getElementById('river-label');
  if (!label) return;
  if      (p < 0.3) label.textContent = 'Swakop River · Namibia · 2019';
  else if (p < 0.6) label.textContent = 'Swakop River · Namibia · 2022';
  else if (p < 0.9) label.textContent = 'Swakop River · Namibia · 2024';
  else              label.textContent = 'Swakop River · Namibia · today';
}

// ── GA4 river milestones ──────────────────────────────────────
const _riverFired = { full: false, low: false, dry: false };
function fireRiverGA(p) {
  if (!window._ga) return;
  if (!_riverFired.full) { _riverFired.full = true; window._ga('river_state', { state: 'full' }); }
  if (p >= 0.3 && !_riverFired.low) { _riverFired.low = true; window._ga('river_state', { state: 'low' }); }
  if (p >= 0.7 && !_riverFired.dry) { _riverFired.dry = true; window._ga('river_state', { state: 'dry' }); }
}

// ── Mobile CSS gradient fallback ──────────────────────────────
function initMobileRiver() {
  const canvas  = document.getElementById('river-canvas');
  const mobileBg = document.getElementById('river-mobile-bg');
  if (canvas) canvas.style.display = 'none';
  if (mobileBg) mobileBg.style.display = 'block';

  window.addEventListener('scroll', () => {
    const total = document.body.scrollHeight - window.innerHeight;
    const p = total > 0 ? window.scrollY / total : 0;
    if (mobileBg) mobileBg.style.setProperty('--river-progress', p);
    updateLabel(p);
  }, { passive: true });
}
