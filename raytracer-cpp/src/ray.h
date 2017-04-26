#pragma once

#include "vec3.h"

class Ray {
    public:
        Vec3 origin, direction;

        Ray() { };

        Ray(const Vec3 &origin, const Vec3 &direction) {
            this->origin = origin;
            this->direction = direction;
        };

        Vec3 point_at_time(float t) const;
};
