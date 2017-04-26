#pragma once

#include "vec3.h"
#include "ray.h"

class Camera {
    public:
        Vec3 origin, lower_left, width, height;

        Camera(const Vec3 &origin, const Vec3 &lower_left, const Vec3 &width, const Vec3 &height) {
            this->origin = origin;
            this->lower_left = lower_left;
            this->width = width;
            this->height = height;
        }
        
        Ray create_ray(float u, float v);
};
