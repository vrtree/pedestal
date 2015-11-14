#version 330 core

uniform vec3 uCamera;


in      vec3 vPosition;
in      vec3 vNormal;
in      vec2 vUV;


out     vec4 fragColor;

void main() {

    fragColor = vec4( 1.);

}